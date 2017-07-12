import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;

/**
 * Created by hakank on 2017-04-20.
 */
// From https://dzone.com/articles/java-8-automatic-memoization
// And it seems to work!
public class Memoizer<T, U> {

    private final Map<T, U> cache = new ConcurrentHashMap<>();

    private Memoizer() {}

    private Function<T, U> doMemoize(final Function<T, U> function) {
        return input -> cache.computeIfAbsent(input, function);
    }

    public static <T, U> Function<T, U> memoize(final Function<T, U> function) {
        return new Memoizer<T, U>().doMemoize(function);
    }
}
