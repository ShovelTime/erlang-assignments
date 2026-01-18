package paradis.assignment3;

import java.util.*;
import java.util.concurrent.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;

public class Program1 {

    final static int NUM_WEBPAGES = 40;
    private static final WebPage[] webPages = new WebPage[NUM_WEBPAGES];

    //Is slightly slower than Program2, on general taking 2.0 seconds to execute on an i5-8300H
    private void exec() throws InterruptedException {
        initialize();
        //long baseStart = System.nanoTime();
        //ArrayList<ArrayBlockingQueue<WebPage>> endpoints = new ArrayList<>(NUM_WEBPAGES);
        //List<WebPage> results = runBaseExecution();
        //long baseEnd = System.nanoTime();

        //if(results.size() != NUM_WEBPAGES) throw new RuntimeException("Invalid base result! result size: " + results.size() + " compared to invariant " + NUM_WEBPAGES);
        //presentResult(results);

        long sharedStart = System.nanoTime();
        List<WebPage> results = runSharedExecution();
        long sharedEnd = System.nanoTime();

        if(results.size() != NUM_WEBPAGES) throw new RuntimeException("Invalid shared result! result size: " + results.size() + " compared to invariant " + NUM_WEBPAGES);
        presentResult(results);

        //System.out.println("Base Execution time (seconds): " + (baseEnd - baseStart) / 1.0E9);
        System.out.println("Execution time (seconds): " + (sharedEnd - sharedStart) / 1.0E9);

    }

    //Multi Produce Multi Consumer, the queues between execution stages are shared across all tasks, meaning data can be passed between parent task.
    //No real difference in execution speed compared to the Base solution, as any speed increase is likely attributed to the cache state being potentially better for one of the 2 methods.
    private List<WebPage> runSharedExecution() throws InterruptedException {
        ForkJoinPool forkJoinPool = new ForkJoinPool();

        ArrayList<SharedBaseForkExecutable> executables = new ArrayList<>(NUM_WEBPAGES);
        ArrayList<Consumer<WebPage>> consumers = new ArrayList<>(Arrays.asList(
                Program1::downloadWebPages,
                Program1::analyzeWebPages,
                Program1::categorizeWebPages
        ));

        ArrayList<ArrayBlockingQueue<WebPage>> queues = new ArrayList<>(consumers.size() + 1);
        for (int i = 0; i < consumers.size() + 1; i++) {
            queues.add(new ArrayBlockingQueue<>(NUM_WEBPAGES));
        }

        ArrayBlockingQueue<WebPage> startQueue = queues.getFirst();

        List<ArrayBlockingQueue<WebPage>> queuesView = Collections.unmodifiableList(queues);
        List<Consumer<WebPage>> consumerViews = Collections.unmodifiableList(consumers);

        for(int i = 0; i < NUM_WEBPAGES; i++) {
            SharedBaseForkExecutable executable = new SharedBaseForkExecutable(queuesView, consumerViews);
            executables.add(executable);
            forkJoinPool.execute(executable);
            startQueue.put(webPages[i]);
        }

        ArrayBlockingQueue<WebPage> endQueue = queues.getLast();
        ArrayList<WebPage> results = new ArrayList<>(NUM_WEBPAGES);
        executables.stream().map(SharedBaseForkExecutable::join).forEach((_) -> {
            try {
                results.add(endQueue.take());
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        });

        return results;
    }


    /*
    //Single producer Single Consumer variant, every base task manages their own queue pool for a particular website.
    private List<WebPage> runBaseExecution() {
        ForkJoinPool forkJoinPool = new ForkJoinPool();

        ArrayList<BaseForkExecutable> executables = new ArrayList<>(NUM_WEBPAGES);
        ArrayList<Consumer<WebPage>> consumers = new ArrayList<>(Arrays.asList(
                Program1::downloadWebPages,
                Program1::analyzeWebPages,
                Program1::categorizeWebPages
        ));

        List<Consumer<WebPage>> consumersView = Collections.unmodifiableList(consumers);
        synchronized (webPages) {
            for (int i = 0; i < NUM_WEBPAGES; i++) {
                WebPage currentWebPage = webPages[i];
                BaseForkExecutable executable = new BaseForkExecutable(currentWebPage, consumersView);
                executables.add(executable);
                forkJoinPool.execute(executable);
            }
        }

        List<WebPage> results = executables.stream().map(ForkJoinTask::join).toList();
        return results;
    }
    */
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
        new Program1().exec();
    }
}

class ChainTaskExecutable extends RecursiveAction {
    private final ArrayBlockingQueue<WebPage> from, to;
    private final Consumer<WebPage> function;
    ChainTaskExecutable(ArrayBlockingQueue<WebPage> from, ArrayBlockingQueue<WebPage> to, Consumer<WebPage> function) {
        this.from = from;
        this.to = to;
        this.function = function;
    }

    @Override
    protected void compute() {
        try {
            WebPage fromPage = from.take();
            function.accept(fromPage);
            to.put(fromPage);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }

    }
}

class BaseForkExecutable extends RecursiveTask<WebPage> {
    private final WebPage targetPage;
    private final List<Consumer<WebPage>> functions;

    BaseForkExecutable(WebPage targetPage, List<Consumer<WebPage>> functions) {
        this.targetPage = targetPage;
        this.functions = functions;
    }

    @Override
    protected WebPage compute() {
        ArrayList<ArrayBlockingQueue<WebPage>> queues = new ArrayList<>(functions.size() + 1);
        ArrayList<ForkJoinTask<Void>> tasks = new ArrayList<>(functions.size());
        for(int i = 0; i < functions.size() + 1; i++) {
            queues.add(new ArrayBlockingQueue<>(1));
        }
        for (int i = 0; i < functions.size(); i++) {
            ChainTaskExecutable task = new ChainTaskExecutable(queues.get(i), queues.get(i + 1), functions.get(i));
            tasks.add(task);
        }

        try {
            queues.getFirst().put(targetPage);
            invokeAll(tasks);
            return queues.getLast().take();
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
}

class SharedBaseForkExecutable extends RecursiveAction {
    private final List<Consumer<WebPage>> functions;
    private final List<ArrayBlockingQueue<WebPage>> queues;

    SharedBaseForkExecutable(List<ArrayBlockingQueue<WebPage>> queues, List<Consumer<WebPage>> functions) {
        this.queues = queues;
        this.functions = functions;
    }

    @Override
    protected void compute() {
        ArrayList<ForkJoinTask<Void>> tasks = new ArrayList<>(functions.size());
        for (int i = 0; i < functions.size(); i++) {
            ChainTaskExecutable task = new ChainTaskExecutable(queues.get(i), queues.get(i + 1), functions.get(i));
            tasks.add(task);
        }

        invokeAll(tasks);
    }

}