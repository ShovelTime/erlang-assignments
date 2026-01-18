import java.math.BigInteger;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Scanner;

public class Factorizer{

    private static ArrayList<FactorizeThread> threadPool;

    private void run(String[] args) throws InterruptedException {
        Scanner input = new Scanner(System.in);
        BigInteger factor = input.nextBigInteger();
        int threadCount = input.nextInt();
        if (threadCount < 1)
        {
            System.out.println("Invalid thread count! please input a valid positive thread amount");
            System.exit(-1);
        }
        threadPool = new ArrayList<>(threadCount);
        ParitionedValues valueSet = partitionRange(factor ,threadCount);
        for(int i = 0; i < threadCount; i++)
        {
            FactorizeThread thread = new FactorizeThread(factor, valueSet.mins[i], valueSet.max[i]);
            threadPool.add(thread);
            thread.start();

        }

        while(true) {}
    }

    public static void main(String[] args) throws InterruptedException {
        new Factorizer().run(args);

    }

    private static ParitionedValues partitionRange(BigInteger factor, int threadCount)
    {
        BigInteger[] mins = new BigInteger[threadCount];
        BigInteger[] max = new BigInteger[threadCount];
        BigInteger incrementStep = factor.divide(BigInteger.valueOf(threadCount));
        for(int i = 0; i < threadCount; i++)
        {
             BigInteger min = BigInteger.valueOf(i);
             mins[i] = incrementStep.multiply(min).add(BigInteger.valueOf(1));
             max[i] = incrementStep.multiply(min).add(incrementStep);
        }
        return new ParitionedValues(mins, max);
    }

    synchronized static void reportFactors(BigInteger factor1, BigInteger factor2, Instant startTime, FactorizeThread callingThread)
    {
        if(factor1 == null || factor2 == null)
        {
            if(!areOtherThreadsAlive(callingThread))
            {
                System.out.println("No Factorization possible");
            }
            else return;
        }
        else {
            if(factor1.equals(BigInteger.ONE) || factor2.equals(BigInteger.ONE)) return;
            Instant stoppingTime = Instant.now();
            Duration totalTime = Duration.between(startTime, stoppingTime);
            System.out.println("Factors Found! : " + factor1 + "\n\nand\n\n" + factor2 + "\n\nComputation Time: " + totalTime.toString());
        }
        System.exit(0);

    }

    private static boolean areOtherThreadsAlive(FactorizeThread callerThread)
    {
        for (FactorizeThread thread : threadPool)
        {
            if(!thread.isDone() || thread == callerThread) continue;
            return true;
        }
        return false;
    }

    record ParitionedValues(BigInteger[] mins, BigInteger[] max)
    { }

    static class FactorizeThread extends Thread{
        private boolean done = false;
        private final BigInteger product;
        private final BigInteger min;
        private final BigInteger max;

        FactorizeThread(BigInteger product, BigInteger min, BigInteger max)
        {
            this.product = product;
            this.min = min;
            this.max = max;
        }

        @Override
        public void run() {
            Instant time = Instant.now();
            BigInteger number = min;
            while (number.compareTo(max) <= 0) {
                if (product.remainder(number).compareTo(BigInteger.ZERO) == 0) {
                    BigInteger factor2 = product.divide(number);
                    Factorizer.reportFactors(number ,factor2, time, this);
                }
                number = number.add(BigInteger.ONE);
            }
            Factorizer.reportFactors(null, null, time, this);
            setDone();
        }

        synchronized private void setDone()
        {
            done = true;
        }

        synchronized public boolean isDone()
        {
            return done;
        }
    }


}
