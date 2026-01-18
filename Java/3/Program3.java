package paradis.assignment3;

import java.util.*;
import java.util.concurrent.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;

public class Program3 {

    final static int NUM_WEBPAGES = 40;
    private static final WebPage[] webPages = new WebPage[NUM_WEBPAGES];

    //Is slightly slower than Program2, on general taking 2.0 seconds to execute on an i5-8300H
    private void exec() throws InterruptedException {
        initialize();
        long sharedStart = System.nanoTime();
        List<WebPage> results = runSharedExecution();
        long sharedEnd = System.nanoTime();

        if(results.size() != NUM_WEBPAGES) throw new RuntimeException("Invalid shared result! result size: " + results.size() + " compared to invariant " + NUM_WEBPAGES);
        presentResult(results);

        System.out.println("Shared Execution time (seconds): " + (sharedEnd - sharedStart) / 1.0E9);

    }

    private List<WebPage> runSharedExecution() throws InterruptedException {
        MyExecutor executor = new MyExecutor();

        ArrayList<ChainExecutable> executables = new ArrayList<>(NUM_WEBPAGES);

        ArrayList<ArrayBlockingQueue<WebPage>> queues = new ArrayList<>(4);
        for (int i = 0; i < 4; i++) {
            queues.add(new ArrayBlockingQueue<>(NUM_WEBPAGES));
        }

        ArrayBlockingQueue<WebPage> startQueue = queues.getFirst();

        List<ArrayBlockingQueue<WebPage>> queuesView = Collections.unmodifiableList(queues);

        for(int i = 0; i < NUM_WEBPAGES; i++) {
            executor.execute(new ChainExecutable(startQueue, queuesView.get(1), Program3::downloadWebPages));
            executor.execute(new ChainExecutable(queuesView.get(1), queuesView.get(2), Program3::analyzeWebPages));
            executor.execute(new ChainExecutable(queuesView.get(2), queuesView.get(3), Program3::categorizeWebPages));

            startQueue.put(webPages[i]);
        }

        ArrayBlockingQueue<WebPage> endQueue = queues.getLast();
        ArrayList<WebPage> results = new ArrayList<>(NUM_WEBPAGES);
        for(int i = 0; i < NUM_WEBPAGES; i++) {
            results.add(endQueue.take());
        }
        executor.shutdown(); //stop threads.
        return results;
    }

    private static void initialize() {
        synchronized (webPages)
        {
            for (int i = 0; i < NUM_WEBPAGES; i++) {
                webPages[i] = new WebPage(i, "http://www.site.se/page" + i + ".html");
            }
        }

    }

    // [Do modify this sequential part of the program.]
    private static void downloadWebPages(WebPage webPage) {
        synchronized (webPage) {
            webPage.download();
        }

    }

    // [Do modify this sequential part of the program.]
    private static void analyzeWebPages(WebPage webPage) {
        synchronized (webPage) {
            webPage.analyze();
        }
    }

    // [Do modify this sequential part of the program.]
    private static void categorizeWebPages(WebPage webPage) {
        synchronized (webPage) {
            webPage.categorize();
        }
    }

    // [You are welcome to modify this method, but it should NOT be parallelized.]
    private static void presentResult(List<WebPage> completedWebPages) {
        synchronized (webPages) {
            for (WebPage webPage : completedWebPages) {
                System.out.println(webPage);
            }
        }

    }

    public static void main(String[] args) throws InterruptedException {
        new Program3().exec();
    }
}



class ChainExecutable implements Runnable {
    private final ArrayBlockingQueue<WebPage> from, to;
    private final Consumer<WebPage> function;

    ChainExecutable(ArrayBlockingQueue<WebPage> from, ArrayBlockingQueue<WebPage> to, Consumer<WebPage> function) {
        this.from = from;
        this.to = to;
        this.function = function;
    }

    @Override
    public void run() {
        try {
            WebPage fromPage = from.take();
            function.accept(fromPage);
            to.put(fromPage);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }

    }
}




class MyExecutor implements ExecutorService {
    private final ArrayList<ExecutorThread> threadPool;
    private final LinkedBlockingQueue<FutureTask<?>> waitingTasks = new LinkedBlockingQueue<>();
    private volatile boolean shutdown = false;


    MyExecutor(int threadCount) {
        this.threadPool = new ArrayList<>(threadCount);
        for (int i = 0; i < threadCount; i++) {
            ExecutorThread thread = new ExecutorThread(this,  waitingTasks);
            thread.start();
            threadPool.add(thread);

        }
    }
    MyExecutor() {
        this(Runtime.getRuntime().availableProcessors());
    }

    @Override
    public void shutdown() {
        shutdown = true;
        for (ExecutorThread thread : threadPool) {
            thread.interrupt(); // notify potentially waiting threads.
        }
    }

    @Override
    public List<Runnable> shutdownNow() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public boolean isShutdown() {
        return shutdown;
    }

    @Override
    public boolean isTerminated() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public boolean awaitTermination(long l, TimeUnit timeUnit) throws InterruptedException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public <T> Future<T> submit(Callable<T> callable) {
        FutureTask<T> futureTask = new FutureTask<>(callable);
        this.waitingTasks.add(futureTask);
        return futureTask;
    }

    @Override
    public <T> Future<T> submit(Runnable runnable, T t) {
        FutureTask<T> futureTask = new FutureTask<>(runnable, t);
        this.waitingTasks.add(futureTask);
        return futureTask;
    }

    @Override
    public Future<?> submit(Runnable runnable) {
        FutureTask<?> futureTask = new FutureTask<>(runnable, null);
        this.waitingTasks.add(futureTask);
        return futureTask;
    }

    @Override
    public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> collection) throws InterruptedException {
        List<FutureTask<T>> tasks = collection.stream().map(FutureTask::new).toList();
        List<Future<T>> futures = tasks.stream().map(task -> (RunnableFuture<T>) task).collect(Collectors.toList());
        this.waitingTasks.addAll(tasks);
        for(Future<T> future : futures) {
            try {
                future.get();
            } catch (ExecutionException e) {
                throw new RuntimeException(e);
            }
        }

        return futures;
    }

    @Override
    public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> collection, long l, TimeUnit timeUnit) throws InterruptedException {
        List<FutureTask<T>> tasks = collection.stream().map(FutureTask::new).toList();
        List<Future<T>> futures = tasks.stream().map(task -> (RunnableFuture<T>) task).collect(Collectors.toList());
        this.waitingTasks.addAll(tasks);

        long nanoTimeRemaining = timeUnit.toNanos(l);

        while(nanoTimeRemaining > 0) {
            long start = System.nanoTime();
            try {
                tasks.removeFirst().get(nanoTimeRemaining, TimeUnit.NANOSECONDS);
                long end = System.nanoTime();
                long duration = end - start;
                nanoTimeRemaining -= duration;
            } catch (ExecutionException | TimeoutException e) {
                return Collections.unmodifiableList(tasks);
            }
        }

        return Collections.unmodifiableList(tasks);
    }

    @Override
    public <T> T invokeAny(Collection<? extends Callable<T>> collection) throws InterruptedException, ExecutionException {
        return null;
    }

    @Override
    public <T> T invokeAny(Collection<? extends Callable<T>> collection, long l, TimeUnit timeUnit) throws InterruptedException, ExecutionException, TimeoutException {
        return null;
    }

    @Override
    public void execute(Runnable runnable) {
        FutureTask<Void> futureTask = new FutureTask<>(runnable, null);
        this.waitingTasks.add(futureTask);

    }
}

class ExecutorThread extends Thread {
    private final MyExecutor executor;
    private final LinkedBlockingQueue<FutureTask<?>> taskQueue;
    private volatile boolean terminated = false;

    ExecutorThread(MyExecutor executor, LinkedBlockingQueue<FutureTask<?>> taskQueue) {
        this.executor = executor;
        this.taskQueue = taskQueue;
    }

    public boolean isTerminated() {
        return terminated;
    }

    public void run() {
        while (!executor.isShutdown()) {
            try {

                FutureTask<?> task = taskQueue.take();
                task.run();
            } catch (InterruptedException _) {
            }
        }
        this.terminated = true;
    }
}
