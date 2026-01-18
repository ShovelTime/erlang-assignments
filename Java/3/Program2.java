// Peter Idestam-Almquist, 2020-02-04.
// [Replace this comment with your own name.]

// [Do necessary modifications of this file.]

package paradis.assignment3;

// [You are welcome to add some import statements.]

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

//Is slightly faster than Program1, on general taking 1.7 seconds to execute on an i5-8300H
public class Program2 {
    final static int NUM_WEBPAGES = 40;
    private static final WebPage[] webPages = new WebPage[NUM_WEBPAGES];
    // [You are welcome to add some variables.]

    // [You are welcome to modify this method, but it should NOT be parallelized.]
    private static void initialize() {
        synchronized (webPages) {
            for (int i = 0; i < NUM_WEBPAGES; i++) {
                webPages[i] = new WebPage(i, "http://www.site.se/page" + i + ".html");
            }
        }
    }

    // [Do modify this sequential part of the program.]
    private static void downloadWebPage(WebPage webPage) {
        synchronized (webPage) {
            webPage.download();
        }
    }

    // [Do modify this sequential part of the program.]
    private static void analyzeWebPage(WebPage webPage) {
        synchronized (webPage) {
            webPage.analyze();
        }
    }

    // [Do modify this sequential part of the program.]
    private static void categorizeWebPage(WebPage webPage) {
        synchronized (webPage) {
            webPage.categorize();
        }
    }

    // [You are welcome to modify this method, but it should NOT be parallelized.]
    private static void presentResult(List<String> results) {
        for(String result : results ) {
            System.out.println(result);
        }
    }

    public static void main(String[] args) {
        // Initialize the list of webpages.
        initialize();

        // Start timing.
        long start = System.nanoTime();

        List<WebPage> pages = List.of(webPages);
        List<String> processedPages = pages.parallelStream()
                .peek(Program2::downloadWebPage)
                .peek(Program2::analyzeWebPage)
                .peek(Program2::categorizeWebPage)
                .map(WebPage::toString).toList();

        // Stop timing.
        long stop = System.nanoTime();
        // Present the result.
        presentResult(processedPages);
        // Present the execution time.
        System.out.println("Execution time (seconds): " + (stop - start) / 1.0E9);
    }
}
