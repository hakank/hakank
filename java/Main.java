import java.io.BufferedReader;
import java.io.File;
import java.io.StringReader;
import java.math.BigInteger;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.util.*;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.Path;

// Unused but needed since IntelliJ warns for upper case variables
import static javafx.scene.input.KeyCode.I;
import static javax.swing.text.html.HTML.Attribute.N;


/*
   Project Euler problems 1..50.
   Goal: All problems should run in < 1s.
   Exceptions:
     - #10: 1.11s
     - #14: 1.06s
     - #43: 1.03s
   Most methods run in < 0.1s and many in < 0.01s.

   Created by Håkan Kjellerstrand (hakank@gmail.com)
   Also see my Java page: http://hakank.org/java/
   And my Project Euler page: http://hakank.org/project_euler/

 */
class Main {

	public static void main(String[] args) {

		long start_time = System.nanoTime();
		EulerUtils.init_answers();

		// Note: since I use reflection to run the methods,
		// they are marked as "never used" by IntelliJ.
		//
		List<String> functions = Arrays.asList(


				// "euler1", // 0.0064s
				"euler1b", // 0.00066s

				// "euler2",  // 0.0003s
				// "euler2a", // 0.001s
				// "euler2b", // 0.002s
				// "euler2c", // 0.002s
				// "euler2d", // 0.003s
				// "euler2e", // 0.005s
				// "euler2f", // 0.0038s
				// "euler2g", // 0.0016s
				// "euler2h", // 0.003s testing Memoizer
				"euler2i", // 0.002s caching with computeIfAbsent

				// "euler3",  // 0.070s
				// "euler3b", // 0.060s
				// "euler3c", // 0.057s
				"euler3d", // 0.050s

				// "euler4", // 0.0341s
				"euler4b", // 0.031s
				// "euler4c", // 0.0526s
				// "euler4d", // 0.044s
				// "euler4e", // 0.0404s

				// "euler5", // 0.0024s
				"euler5b", // 0.0003s

				// "euler6",  // 0.0004s
				// "euler6b", // 0.0006s
				"euler6c", // 0.00007s

				"euler7", // 0.197s

				"euler8", // 0.0007s

				"euler9", // 0.0189s
				// "euler9b", // 0.1966s

				"euler10", // 1.06s
				// "euler10b", // too slow (2.8s)
				// "euler10c", // 1.16s (but after euler10 has run: 0.59s)
				// "euler10d", // 12.5s!

				"euler11", // 0.0034s

				"euler12", // 0.0559s

				"euler13", // 0.00147s

				"euler14", // 1.068s !!!!

				"euler15", // 0.0012s

				"euler16", // 0.0005s
				// "euler16b", // 0.0021s
				// "euler16c", // 0.00058s

				// "euler17", // 0.0061s
				"euler17b", // 0.0021s

				"euler18", // 0.0011s

				// "euler19", // 0.0134s
				"euler19b", // 0.0036s

				"euler20", // 0.0004s
				// "euler20b", // 0.0007s

				"euler21", // 0.0125s
				"euler21b",
				"euler21c",
				"euler21d",

				// "euler22", // 0.0335s
				"euler22b", // 0.0214s

				"euler23", // 0.073s


				// "euler24", // 0.0168s
				"euler24b", // 0.0001s
				// "euler24c" // slow: 1s

				// "euler25", // 0.0607s
				// "euler25b", // 0.93s
				"euler25c", // 0.0148s

				"euler26", // 0.004s
				// "euler26b" // 0.009s

				"euler27",

				"euler28", // 0.0001s
				// "euler28b", // 0.006s
				"euler28c",

				// "euler29", // 0.009s
				"euler29b", // 0.003s

				// "euler30", // 0.3476s
				"euler30b", // 0.3455s

				"euler31", // 0.0117s (uncached)

				"euler32", //  0.131s
				// "euler32b" //  0.151s

				"euler33",  // 0.0002s

				"euler34", // 0.03s

				"euler35", // 0.276s

				"euler36", // 0.057s
				// "euler36b" // 0.0664s

				"euler37", // 0.085s

				"euler38", // 0.0167s
				// "euler38b", // 0.0168s

				"euler39", // 0.054s

				"euler40",  // 0.015

				"euler41",  // 0.0103s

				"euler42", // 0.0169s
				// "euler42b"  // 0.032s

				"euler43",  // 0.955s!!!
				// "euler43b", // 1.810s !!!

				"euler44", // 0.072s

				"euler45", // 0.045s

				"euler46", // 0.0143s

				"euler47", // 0.0154s

				"euler48", // 0.114s

				"euler49", // 0.0625s

				"euler50"  // 0.0293s
		);

		functions.forEach(
				EulerUtils::testEuler
		);

		System.out.println("Total time: " + (System.nanoTime() - start_time) / 1000000000.0);
		System.out.printf("Sum of individual times: %10.10f\n", EulerUtils.total_time);

		System.out.printf("\nNumber of errors: %d\n", EulerUtils.num_errors);
		if (EulerUtils.num_errors > 0) {
			for (String p : EulerUtils.error_problems) {
				System.out.println("\t" + p);
			}

		}
		System.out.println("\nSlow problems:");
		System.out.println("#problems >= 1s: " + EulerUtils.num_more_1s);
		for (String p : EulerUtils.num_more_1s_l) {
			System.out.println("\t" + p);
		}

		System.out.println("#problems >= 0.1s: " + EulerUtils.num_more_01s);
		for (String p : EulerUtils.num_more_01s_l) {
			System.out.println("\t" + p);
		}

	}


	/**************************************************************************
	 Euler problem 1
	 """
	 If we list all the natural numbers below 10 that are multiples of 3 or 5,
	 we get 3, 5, 6 and 9. The sum of these multiples is 23.
	 Find the sum of all the multiples of 3 or 5 below 1000.

	 ****************************************************************************/
	@SuppressWarnings("unused")
	public static String euler1() {

		IntStream values = IntStream.rangeClosed(1, 999);
		return Integer.toString(
				values
						.filter(e -> e % 3 == 0 || e % 5 == 0)
						.sum() // .reduce(0, (c, e) -> c + e)
		);
	}

	@SuppressWarnings("unused")
	public static String euler1b() {
		// skipping declaration of values
		return Integer.toString(
				IntStream.rangeClosed(1, 999)
						.filter(e -> e % 3 == 0 || e % 5 == 0)
						.sum()
		);

	}

	/*********************************************************************************
	 Problem 2
	 """
	 Each new term in the Fibonacci sequence is generated by adding the
	 previous two terms. By starting with 1 and 2, the first 10 terms will be:

	 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

	 Find the sum of all the even-valued terms in the sequence which do not
	 exceed four million.
	 """

	 ***********************************************************************************/

	// Pure imperative
	@SuppressWarnings("unused")
	public static String euler2() {
		Map<Integer, Integer> map = new HashMap<>();
		int i = 1;
		int sum = 0;
		int f = EulerUtils.fib(i, map);
		while (f < 4_000_000) {
			if (f % 2 == 0) {
				sum += f;
			}
			i += 1;
			f = EulerUtils.fib(i, map);
		}
		return Integer.toString(sum);
	}

	// Using array based fib: 0.000089s which seems to be the fastest
	@SuppressWarnings("unused")
	public static String euler2a() {
		int i = 1;
		int sum = 0;
		int f = EulerUtils.fibA(i);
		while (f < 4_000_000) {
			if (f % 2 == 0) {
				sum += f;
			}
			i += 1;
			f = EulerUtils.fibA(i);
		}
		return Integer.toString(sum);
	}


	// Streams (and a bit of cheating)
	@SuppressWarnings("unused")
	public static String euler2b() {
		Map<Integer, Integer> map2 = new HashMap<>();
		return Integer.toString(
				IntStream.rangeClosed(0, 40)
						.filter(i -> EulerUtils.fib(i, map2) % 2 == 0)
						.map(i -> EulerUtils.fib(i, map2))
						.filter(f -> f < 4_000_000)
						.sum()
		);
	}

	// BigInteger (which is not really needed but nice to train on)
	@SuppressWarnings("unused")
	public static String euler2c() {
		Map<BigInteger, BigInteger> map = new HashMap<>();
		BigInteger i = BigInteger.ONE;
		BigInteger sum = i;
		BigInteger f = EulerUtils.fibB(i, map);
		while (f.compareTo(BigInteger.valueOf(4_000_000)) < 1) {
			if (f.mod(BigInteger.valueOf(2)).compareTo(BigInteger.ZERO) == 1) {
				sum = sum.add(f);
			}
			i = i.add(BigInteger.ONE);
			f = EulerUtils.fibB(i, map);
		}
		return sum.toString();
	}

	// BigInteger slightly more streamish...
	@SuppressWarnings("unused")
	public static String euler2d() {
		Map<BigInteger, BigInteger> map = new HashMap<>();
		BigInteger i = BigInteger.ZERO;
		BigInteger two = BigInteger.valueOf(2);
		BigInteger f = EulerUtils.fibB(i, map);
		List<BigInteger> x = new ArrayList<>();
		while (f.compareTo(BigInteger.valueOf(4_000_000)) < 1) {
			if (f.mod(two).compareTo(BigInteger.ZERO) == 1) {
				x.add(f);
			}
			i = i.add(BigInteger.ONE);
			f = EulerUtils.fibB(i, map);
		}
		return (x.stream().reduce(BigInteger::add).get()).toString();
	}

	// Plain Streams (variant of euler2b, but still cheating)
	// Don't know how to terminate a stream. Why don't Java 8 streams have takeWhile()?
	// (Java 9 does, though.)
	@SuppressWarnings("unused")
	public static String euler2e() {
		Map<Integer, Integer> map = new HashMap<>();
		return Integer.toString(
				IntStream.range(0, 40)
						.map(i -> EulerUtils.fib(i, map))
						.filter(f -> f % 2 == 0 && f < 4_000_000)
						.sum()
		);

	}

	// Stream solution using iterate() but with a hack (allMatch and a List sum)
	@SuppressWarnings("unused")
	public static String euler2f() {
		Map<Integer, Integer> map = new HashMap<>();
		List<Integer> sum = new ArrayList<>();
		IntStream.iterate(1, n -> n + 1)
				.filter(i -> EulerUtils.fib(i, map) % 2 == 0 && EulerUtils.fib(i, map) < 4_000_000)
				.map(i -> {
					sum.add(EulerUtils.fib(i, map));
					return EulerUtils.fib(i, map);
				})
				// This is a hack: the next value is -1109825406 and thus we fail (and terminate)
				// also we cant use sum() on this since it's a (boolean) terminal.
				.allMatch(f -> f < 4_000_000 && f > 0);
		sum.remove(sum.size() - 1); // We remove the offending last value
		return Integer.toString(sum.stream().reduce((a, c) -> a + c).get());

	}

	// Same stream hack as euler2f but we map to fib earlier
	@SuppressWarnings("unused")
	public static String euler2g() {
		Map<Integer, Integer> map = new HashMap<>();
		List<Integer> sum = new ArrayList<>();
		IntStream.iterate(1, n -> n + 1)
				.map(i -> EulerUtils.fib(i, map))
				.filter(f -> f % 2 == 0 && f < 4_000_000)
				.map(f -> {
					sum.add(f);
					return f;
				})
				.allMatch(f -> f < 4_000_000 && f > 0);
		sum.remove(sum.size() - 1); // We remove the offending last value
		return Integer.toString(sum.stream().reduce((a, c) -> a + c).get());

	}


	// Testing Memoizer
	@SuppressWarnings("unused")
	public static String euler2h() {
		// Function<Integer, Integer> f = EulerUtils::fibA;
		// Function<Integer, Integer> f = Main::fib_test;
		// Function<Integer, Integer> g = Memoizer.memoize(f);
		// This works as well (i.e. without assigning to a variable)
		Function<Integer, Integer> g = Memoizer.memoize(EulerUtils::fibA);

		// This don't work: Got "variable fibt might not have been initialized"
		// UnaryOperator<Integer> fibt = x -> x <= 1 ? 1 :
		//							(int)fibt.apply(x - 1) + (int)fibt.apply(x-2);
		// Function<Integer, Integer> g = Memoizer.memoize(fibt);

		int i = 1;
		int sum = 0;
		int fib = g.apply(i);
		while (fib < 4_000_000) {
			if (fib % 2 == 0) {
				sum += fib;
			}
			// i += 1;
			fib = g.apply(++i);
		}
		return Integer.toString(sum);
	}

	// Using computeIfAbsent seems to be a little faster than via Memoize
	@SuppressWarnings("unused")
	public static String euler2i() {
		Map<Integer, Integer> map = new HashMap<>();
		return Integer.toString(
				IntStream.range(0, 40)
						.map(i -> map.computeIfAbsent(i, j -> EulerUtils.fibA(j)))
						.filter(f -> f % 2 == 0 && f < 4_000_000)
						.sum()
		);
	}


	/***************************************************************************
	 Euler 3
	 """
	 The prime factors of 13195 are 5, 7, 13 and 29.
	 What is the largest prime factor of the number 600851475143 ?
	 """
	 Note: It requires BigInteger
	 ****************************************************************************/
	@SuppressWarnings("unused")
	public static String euler3() {
		BigInteger n = new BigInteger("600851475143");
		BigInteger two = BigInteger.valueOf(2);
		BigInteger i = two;
		BigInteger d = EulerUtils.sqrt(n);

		BigInteger maxFactor = BigInteger.ZERO;
		// Checking divisibility by 2 first
		if (n.mod(i).compareTo(BigInteger.ZERO) == 0) {
			maxFactor = i;
		}
		// Check from 3 and odd values
		i = BigInteger.valueOf(3);
		while (i.compareTo(d) == -1) {
			if (n.mod(i).compareTo(BigInteger.ZERO) == 0 && i.isProbablePrime(100)) {
				maxFactor = i;
			}
			i = i.add(two);
		}
		return maxFactor.toString();
	}

	// A little more Streamish
	@SuppressWarnings("unused")
	public static String euler3b() {
		BigInteger n = new BigInteger("600851475143");
		BigInteger two = BigInteger.valueOf(2);
		BigInteger d = EulerUtils.sqrt(n);

		List<BigInteger> factors = new ArrayList<>();
		if (n.mod(two).compareTo(BigInteger.ZERO) == 0) {
			factors.add(two);
		}
		// Checking from 3 and then only odd values
		BigInteger i = BigInteger.valueOf(3);
		while (i.compareTo(d) == -1) {
			if (n.mod(i).compareTo(BigInteger.ZERO) == 0 && i.isProbablePrime(100)) {
				factors.add(i);
			}
			i = i.add(two);
		}
		// Note that there's no max for BigInteger, hence the comparator...
		// And the .get() is for getting around the "Optional[6857]" result.
		return factors.stream().max(Comparator.naturalOrder()).get().toString();
	}

	// Pure stream
	@SuppressWarnings("unused")
	public static String euler3c() {
		BigInteger n = new BigInteger("600851475143");
		BigInteger two = BigInteger.valueOf(2);
		return
				// testing all number from 2.. sqrt(n)
				Integer.toString(IntStream.range(2, EulerUtils.sqrt(n).intValue())
						.filter(i -> i % 2 == 1)
						.filter(i -> (n.mod(BigInteger.valueOf(i)).compareTo(BigInteger.ZERO) == 0 && BigInteger.valueOf(i).isProbablePrime(100)))
						.max().getAsInt());
	}


	// Slightly faster: testing 2, and 3..odd numbers
	@SuppressWarnings("unused")
	public static String euler3d() {
		BigInteger n = new BigInteger("600851475143");
		BigInteger two = BigInteger.valueOf(2);
		return Integer.toString(
				IntStream.concat(
						// Checking divisibility just by 2
						IntStream.rangeClosed(2, 2)
								.filter(i -> n.mod(two).compareTo(BigInteger.ZERO) == 0)
						,
						// and 3 .. just odd numbers
						IntStream.range(3, EulerUtils.sqrt(n).intValue())
								.filter(i -> i % 2 == 1)
								.filter(i -> (n.mod(BigInteger.valueOf(i)).compareTo(BigInteger.ZERO) == 0 &&
										BigInteger.valueOf(i).isProbablePrime(100)))
				)
						.max().getAsInt()

		);
	}


	/*
		Euler 4
		"""
		A palindromic number reads the same both ways. The largest palindrome made
		from the product of two 2-digit numbers is 9009 = 91 × 99.

		Find the largest palindrome made from the product of two 3-digit numbers.
		"""

	 */
	@SuppressWarnings("unused")
	public static String euler4() {
		List<Integer> x = new ArrayList<>();
		for (int i = 100; i <= 999; i++) {
			for (int j = 100; j <= i; j++) {
				int t = i * j;
				if (EulerUtils.isPalindrome(Integer.toString(t))) {
					x.add(t);
				}
			}
		}
		return Integer.toString(x
				.stream()
				.distinct()
				.max(Comparator.naturalOrder()).get());
	}

	@SuppressWarnings("unused")
	public static String euler4b() {
		Map<Integer, Integer> map = new HashMap<>();
		for (int i = 100; i <= 999; i++) {
			for (int j = 100; j <= i; j++) {
				int t = i * j;
				if (EulerUtils.isPalindrome(Integer.toString(t)) && !map.containsKey(t)) {
					map.put(t, 1);
				}
			}
		}
		return Integer.toString(map.keySet()
				.stream()
				.max(Comparator.naturalOrder()).get());
	}


	// (Almost) plain Stream
	@SuppressWarnings("unused")
	public static String euler4c() {
		// string -> int: Integer.parseInt(s)
		// int -> string: Integer.toString(i);
		int from = 100;
		int to = 999;
		// Note: Supplier<>
		Supplier<IntStream> s1 = () -> IntStream.rangeClosed(from, to);
		List<Integer> x = new ArrayList<>();
		// Note the .get()
		s1.get().forEach(
				i -> s1.get().forEach(j -> {
							int t = i * j;
							if (EulerUtils.isPalindrome(Integer.toString(t))) {
								x.add(t);
							}
						}
				)
		);
		return Integer.toString(x
				.stream()
				.distinct()
				//// this was optimized away by IntelliJ from:
				//.max(((o1, o2) -> o1.compareTo(o2)))
				//// to:
				.max(Comparator.naturalOrder()).get());
	}

	// This is pure stream.
	// The solution was to use ,get().flatMap() to collect the
	@SuppressWarnings("unused")
	public static String euler4d() {
		// Note: Supplier<>
		Supplier<IntStream> s1 = () -> IntStream.rangeClosed(100, 999);
		// Note the .get()
		return Integer.toString(
				s1
						.get().flatMap(i ->
						s1.get().map(j -> i * j))
						.filter(t -> EulerUtils.isPalindrome(Integer.toString(t)))
						// .distinct() // slower
						.max().getAsInt()
		);
	}


	@SuppressWarnings("unused")
	public static String euler4e() {
		List<Integer> x = new ArrayList<>();
		int[] range = EulerUtils.range_array(100, 900);
		for (int i : range) {
			for (int j : range) {
				int t = i * j;
				if (EulerUtils.isPalindrome(Integer.toString(t))) {
					x.add(t);
				}
			}
		}
		return Integer.toString(x
				.stream()
				.distinct()
				.max(Comparator.naturalOrder()).get());
	}


	/****************************************************************************
	 Problem 5
	 """
	 2520 is the smallest number that can be divided by each of the numbers
	 from 1 to 10 without any remainder.

	 What is the smallest number that is evenly divisible by all of the numbers
	 from 1 to 20?
	 """
	 *****************************************************************************/
	// Using lcm(2..20)
	@SuppressWarnings("unused")
	public static String euler5() {
		BigInteger a = BigInteger.ONE;
		// Stream<Integer> x = Stream.of(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20);
		// .boxed() is needed to convert an IntStream to Stream<Integer>
		Stream<Integer> x = IntStream.rangeClosed(2, 20).boxed();
		return x
				.map(BigInteger::valueOf) // convert to BigInteger
				.reduce(BigInteger.ONE, EulerUtils::lcmB).toString(); // do the lcm magic;

	}

	// imperative: much faster than the stream version
	@SuppressWarnings("unused")
	public static String euler5b() {
		BigInteger a = BigInteger.ONE;
		for (int i = 2; i <= 20; i++) {
			a = EulerUtils.lcmB(a, BigInteger.valueOf(i));
		}
		return a.toString();

	}


	/**************************************************************************
	 Problem 6
	 """
	 The sum of the squares of the first ten natural numbers is,
	 1^(2) + 2^(2) + ... + 10^(2) = 385

	 The square of the sum of the first ten natural numbers is,
	 (1 + 2 + ... + 10)^(2) = 55^(2) = 3025

	 Hence the difference between the sum of the squares of the first ten
	 natural numbers and the square of the sum is 3025 − 385 = 2640.

	 Find the difference between the sum of the squares of the first one
	 hundred natural numbers and the square of the sum.
	 """

	 ***************************************************************************/
	@SuppressWarnings("unused")
	public static String euler6() {
		int s1 = IntStream.rangeClosed(1, 100).sum();
		int s2 = IntStream.rangeClosed(1, 100).map(i -> i * i).sum();
		return Integer.toString(s1 * s1 - s2);
	}

	@SuppressWarnings("unused")
	public static String euler6b() {
		int s1 = EulerUtils.sum_array(EulerUtils.range_array(1, 100));
		int s2 = IntStream.rangeClosed(1, 100).map(i -> i * i).sum();
		return Integer.toString(s1 * s1 - s2);
	}

	@SuppressWarnings("unused")
	public static String euler6c() {
		int s1 = 0;
		int s2 = 0;
		for (int i : EulerUtils.range_array(1, 100)) {
			s1 += i;
			s2 += i * i;
		}
		return Integer.toString(s1 * s1 - s2);
	}


	/**************************************************************************
	 Problem 7
	 """
	 By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
	 that the 6^(th) prime is 13.

	 What is the 10001^(st) prime number?
	 """
	 **************************************************************************/
	// Note: This is the first problem that took more than 0.1s: 0.22s
	// Since I don't know how to define a takeWhile in Java 8, I skip a Stream version...
	@SuppressWarnings("unused")
	public static String euler7() {
		BigInteger p = BigInteger.valueOf(15); // we got 2,3,5,7,11,13
		int c = 6;
		BigInteger p2 = p;
		int i = 0;
		while (c < 10001) {
			i++;
			if (p.isProbablePrime(10)) {
				c++;
				p2 = p;
			}
			p = p.add(BigInteger.valueOf(2));
		}
		return p2.toString();
	}


	/****************************************************************************
	 Problem 8:
	 """
	 Find the greatest product of five consecutive digits in the
	 1000-digit number.
	 ...
	 """
	 ****************************************************************************/
	@SuppressWarnings("unused")
	public static String euler8() {
		String num = "7316717653133062491922511967442657474235534919493496983" +
				"52031277450632623957831801698480186947885184385861560789112" +
				"9494954595017379583319528532088055111254069874715852386305" +
				"07156932909632952274430435576689664895044524452316173185640" +
				"3098711121722383113622298934233803081353362766142828064444" +
				"86645238749303589072962904915604407723907138105158593079608" +
				"6670172427121883998797908792274921901699720888093776657273" +
				"33001053367881220235421809751254540594752243525849077116705" +
				"5601360483958644670632441572215539753697817977846174064955" +
				"14929086256932197846862248283972241375657056057490261407972" +
				"9686524145351004748216637048440319989000889524345065854122" +
				"75886668811642717147992444292823086346567481391912316282458" +
				"6178664583591245665294765456828489128831426076900422421902" +
				"26710556263211111093705442175069416589604080719840385096245" +
				"5444362981230987879927244284909188845801561660979191338754" +
				"99200524063689912560717606058861164671094050775410022569831" +
				"5520005593572972571636269561882670428252483600823257530420" +
				"752963450";
		int max = 0;
		char[] s = new char[5];
		for (int i = 0; i < num.length() - 4; i++) {
			num.getChars(i, i + 5, s, 0);
			int r = 1;
			for (char c : s) {
				r *= Character.getNumericValue(c);
			}
			if (r > max) {
				max = r;
			}
		}
		return Integer.toString(max);
	}


	/****************************************************************************
	 Problem 9:
	 """
	 A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
	 a^2 + b^2 = c^2

	 For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

	 There exists exactly one Pythagorean triplet for which a + b + c = 1000.
	 Find the product a*b*c.
	 """
	 ****************************************************************************/
	@SuppressWarnings("unused")
	public static String euler9() {
		int prod = 0;
		for (int c = 1; c <= 500; c++) {
			for (int b = 1; b <= c; b++) {
				for (int a = 1; a <= b; a++) {
					if (a + b + c == 1000 && a * a + b * b - c * c == 0) {
						prod = a * b * c;
						break;
					}
				}
			}
		}
		return Integer.toString(prod);
	}

	// Stream version, but - again - it's hard to do while clauses with streams.
	// Many CPU cycles died unnecessary for this: 0.84s
	// However, with parallel(): 0.19s
	@SuppressWarnings("unused")
	public static String euler9b() {
		Supplier<IntStream> s1 = () -> IntStream.rangeClosed(1, 1000);
		Map<String, String> map = new HashMap<>();
		s1.get().parallel().forEach(
				c -> s1.get().forEach(
						b ->
								s1.get().forEach(
										a -> {
											if (a <= b && b < c &&
													a + b + c == 1000 && a * a + b * b - c * c == 0) {
												// cannot return from here ("unexpected return")
												map.put("result", Integer.toString(a * b * c));
											}
										}

								)
				)
		);

		return map.get("result");

	}


	/****************************************************************************
	 Problem 10:
	 """
	 The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

	 Find the sum of all the primes below two million.
	 """
	 ****************************************************************************/
	// Stream version: works but is slow: sequential: 2.87s, parallel: 0.77s
	@SuppressWarnings("unused")
	public static String euler10() {
		return
				IntStream.range(2, 2_000_000)
						.parallel() // much faster 2.87s->0.77s
						.filter(i -> BigInteger.valueOf(i).isProbablePrime(10))
						.mapToObj(BigInteger::valueOf)
						.reduce(BigInteger::add).get().toString();
	}

	// Slightly faster than the non-parallel stream version: 2.79s
	@SuppressWarnings("unused")
	public static String euler10b() {
		BigInteger sum = BigInteger.valueOf(2);
		for (int i = 3; i < 2_000_000; i = i + 2) {
			BigInteger t = BigInteger.valueOf(i);
			if (t.isProbablePrime(10)) {
				sum = sum.add(t);
			}

		}
		return sum.toString();

	}

	// Different order of mapToObj and filter
	@SuppressWarnings("unused")
	public static String euler10c() {
		return
				IntStream.range(2, 2_000_000)
						.parallel() // much faster 2.87s->0.77s
						.mapToObj(BigInteger::valueOf)
						.filter(f -> f.isProbablePrime(10))
						.reduce(BigInteger::add).get().toString();
	}

	// Using BigInteger.nextProbablePrime: 12.5s!
	@SuppressWarnings("unused")
	public static String euler10d() {
		BigInteger t = BigInteger.valueOf(2);
		BigInteger two_mill = BigInteger.valueOf(2_000_000);
		BigInteger sum = t;
		boolean found = false;
		while (!found) {
			t = t.nextProbablePrime();
			if (t.compareTo(two_mill) >= 1) {
				found = true;
			} else {
				sum = sum.add(t);
			}
		}
		return sum.toString();

	}


	/****************************************************************************
	 Problem 11:
	 """
	 In the 20x20 grid below, four numbers along a diagonal line have
	 been marked in red.

	 ...

	 The product of these numbers is 26 x 63 x 78 x 14 = 1788696.

	 What is the greatest product of four adjacent numbers in any direction
	 (up, down, left, right, or diagonally) in the 20 x 20 grid?
	 """
	 ****************************************************************************/
	@SuppressWarnings("unused")
	public static String euler11() {
		int m[][] =
				{{8, 2, 22, 97, 38, 15, 0, 40, 0, 75, 4, 5, 7, 78, 52, 12, 50, 77, 91, 8},
						{49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 4, 56, 62, 0},
						{81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 3, 49, 13, 36, 65},
						{52, 70, 95, 23, 4, 60, 11, 42, 69, 24, 68, 56, 1, 32, 56, 71, 37, 2, 36, 91},
						{22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80},
						{24, 47, 32, 60, 99, 3, 45, 2, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50},
						{32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70},
						{67, 26, 20, 68, 2, 62, 12, 20, 95, 63, 94, 39, 63, 8, 40, 91, 66, 49, 94, 21},
						{24, 55, 58, 5, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72},
						{21, 36, 23, 9, 75, 0, 76, 44, 20, 45, 35, 14, 0, 61, 33, 97, 34, 31, 33, 95},
						{78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 3, 80, 4, 62, 16, 14, 9, 53, 56, 92},
						{16, 39, 5, 42, 96, 35, 31, 47, 55, 58, 88, 24, 0, 17, 54, 24, 36, 29, 85, 57},
						{86, 56, 0, 48, 35, 71, 89, 7, 5, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58},
						{19, 80, 81, 68, 5, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 4, 89, 55, 40},
						{4, 52, 8, 83, 97, 35, 99, 16, 7, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66},
						{88, 36, 68, 87, 57, 62, 20, 72, 3, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69},
						{4, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 8, 46, 29, 32, 40, 62, 76, 36},
						{20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 4, 36, 16},
						{20, 73, 35, 29, 78, 31, 90, 1, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 5, 54},
						{1, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 1, 89, 19, 67, 48}};

		int len = m[1].length;
		int n = 4;
		return Integer.toString(
				IntStream.concat(
						IntStream.concat(
								IntStream.range(0, len)
										.map(i -> EulerUtils.max_running_prod(EulerUtils.get_row(m, i), n))
								,
								IntStream.range(0, len)
										.map(i -> EulerUtils.max_running_prod(EulerUtils.get_col(m, i), n))
						)
						,
						IntStream.concat(
								IntStream.rangeClosed(0, len - n)
										.map(j -> EulerUtils.get_diags1(m, n, j))
								,
								IntStream.rangeClosed(0, len - n)
										.map(j -> EulerUtils.get_diags2(m, n, j))
						)
				)
						.max().getAsInt()
		);


	}

	/****************************************************************************
	 Problem 12:
	 """
	 The sequence of triangle numbers is generated by adding the natural numbers.
	 So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28.
	 The first ten terms would be:

	 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

	 Let us list the factors of the first seven triangle numbers:

	 1: 1
	 3: 1,3
	 6: 1,2,3,6
	 10: 1,2,5,10
	 15: 1,3,5,15
	 21: 1,3,7,21
	 28: 1,2,4,7,14,28

	 We can see that the 7th triangle number, 28, is the first triangle number
	 to have over five divisors.

	 Which is the first triangle number to have over five-hundred divisors?")
	 """
	 ****************************************************************************/
	@SuppressWarnings("unused")
	public static String euler12() {
		int tnum = 0;
		int i = 0;
		long num_divs = 0;
		while (num_divs <= 500 && tnum >= 0) {
			i++;
			tnum += i;
			num_divs = EulerUtils.num_divisors(tnum);
		}
		return Integer.toString(tnum);

	}


	/****************************************************************************
	 Problem 13:
	 """
	 Work out the first ten digits of the sum of the following
	 one-hundred 50-digit numbers.
	 37107287533902102798797998220837590246510135740250
	 ....
	 20849603980134001723930671666823555245252804609722
	 53503534226472524250874054075591789781264330331690")
	 """
	 ****************************************************************************/
	@SuppressWarnings("unused")
	public static String euler13() {
		final String[] nums = EulerUtils.euler13_nums;

		BigInteger num = BigInteger.ONE;
		for (String s : nums) {
			num = num.add(new BigInteger(s));
		}
		String res = num.toString();
		return res.substring(0, 10);
	}

	/****************************************************************************
	 Problem 14:
	 """
	 The following iterative sequence is defined for the set of positive integers:

	 n n/2 (n is even)
	 n 3n + 1 (n is odd)

	 Using the rule above and starting with 13, we generate the following
	 sequence:
	 13 40 20 10 5 16 8 4 2 1

	 It can be seen that this sequence (starting at 13 and finishing at 1)
	 contains
	 10 terms. Although it has not been proved yet (Collatz Problem), it is
	 thought that all starting numbers finish at 1.

	 Which starting number, under one million, produces the longest chain?

	 NOTE: Once the chain starts the terms are allowed to go above one million.)
	 """
	 ****************************************************************************/
	// This takes slightly over 1s (which is my "happy limit" for these katas)
	// But cheating and only consider the odd numbers 1..2..999_999 it takes 0.85s
	@SuppressWarnings("unused")
	public static String euler14() {
		// Map<BigInteger, Integer> lens = new HashMap<>(999_997, (float) 0.5); // might be faster
		Map<BigInteger, Integer> lens = new HashMap<>();
		int maxLen = 0;
		BigInteger one = BigInteger.ONE;
		BigInteger maxN = one;
		int limit = 999_999;
		// cheating a bit...
		// for(int n = 3; n <= limit; n = n+2) {
		// but let's not cheat...
		for (int n = 2; n <= limit; n++) {
			BigInteger nB = BigInteger.valueOf(n);
			BigInteger m = nB;
			int cLen = 1;
			// m > 1
			while (m.compareTo(one) == 1) {
				if (lens.containsKey(m)) {
					// now we know the full length of m, so we don't have to pursue it more.
					cLen += lens.get(m) - 1;
					m = one;
				} else {
					m = EulerUtils.hailstoneB(m);
					cLen++;
				}
			}
			lens.put(nB, cLen);
			if (cLen > maxLen) {
				maxLen = cLen;
				maxN = nB;
			}
		}

		return maxN.toString();

	}

	// Stream version with parallel
	// With ConcurrentHashMap: 21.02s
	// With HashMap: 15.557s
	// With HashMap and cheating (3..2..999_999): 8.16s
	// With HashMap, cheating and parallel and _wrong answer_: 1.198s!
	@SuppressWarnings("unused")
	public static String euler14b() {
		// Much slower than HashMap. Correct for non-parallel, incorrect for parallel
		// Map<BigInteger, BigInteger> lens = new ConcurrentHashMap<>();
		// Map<String, BigInteger> t = new ConcurrentHashMap<>(); // globals

		// These give correct values for non-parallel but wrong for parallel
		Map<BigInteger, BigInteger> lens = new HashMap<>();
		Map<String, BigInteger> t = new HashMap<>();

		BigInteger one = BigInteger.ONE;
		BigInteger zero = BigInteger.ZERO;
		int limit = 999_999;

		t.put("maxN",zero);
		t.put("maxLen",zero);
		t.put("cLen",zero);
		IntStream.rangeClosed(2,limit)
		// Cheating
		// IntStream.iterate(3, n->n+2)
				// .parallel()
				.mapToObj(BigInteger::valueOf)
				// .limit(499_999) // for cheating with iterate(3,n->n+2)
				.forEach(m ->
					{
						t.put("m", m);
						t.put("cLen",one);
						while (t.get("m").compareTo(one) == 1) {
							if (lens.containsKey(t.get("m"))) {
								// now we know the full length of m, so we don't have to pursue it more.
								t.put("cLen", t.get("cLen").add(lens.getOrDefault(t.get("m"), zero).subtract(one)));
								t.put("m",one);
							} else {
								t.put("m", EulerUtils.hailstoneB(t.get("m")));

								t.put("cLen", t.get("cLen").add(one));
								// System.out.println("\tm: " + m + " cLen: " + t.get("cLen"));
							}
						}
						lens.put(t.get("m"), t.get("cLen"));
						if (t.get("cLen").compareTo(t.get("maxLen")) == 1) {
								// System.out.println("Found new maxN: " + m + " cLen: " + t.get("cLen"));
								t.put("maxLen", t.get("cLen"));
								t.put("maxN", m);
						}
					}
				);
		// System.out.println("maxN: " + t.get("maxN") + " maxLen: " + t.get("maxLen"));
		return t.getOrDefault("maxN",zero).toString();

	}


	/****************************************************************************
	 Problem 15:
	 """
	 Starting in the top left corner of a 2×2 grid, there are 6 routes
	 (without backtracking) to the bottom right corner.

	 How many routes are there through a 20×20 grid?
	 """
	 ****************************************************************************/
	@SuppressWarnings("unused")
	public static String euler15() {
		BigInteger x = IntStream.rangeClosed(21, 40).boxed().map(BigInteger::valueOf).reduce(BigInteger::multiply).get();
		BigInteger y = IntStream.rangeClosed(2, 20).boxed().map(BigInteger::valueOf).reduce(BigInteger::multiply).get();
		return (x.divide(y)).toString();
	}

	/****************************************************************************
	 Problem 16:
	 """
	 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

	 What is the sum of the digits of the number 2^1000?
	 """
	 ****************************************************************************/
	@SuppressWarnings("unused")
	public static String euler16() {
		BigInteger x = BigInteger.valueOf(2).pow(1000);
		String s = x.toString();
		int sum = 0;
		for (char c : s.toCharArray()) {
			sum += Character.getNumericValue(c);
		}
		return Integer.toString(sum);
	}

	@SuppressWarnings("unused")
	public static String euler16b() {
		BigInteger x = BigInteger.valueOf(2).pow(1000);
		return Integer.toString(
				Stream.of(x.toString().chars())
						.flatMapToInt(i -> i) // note that this is needed since .char is an IntStream
						.map(Character::getNumericValue)
						.sum()
		);
	}

	@SuppressWarnings("unused")
	public static String euler16c() {
		IntStream x = BigInteger.valueOf(2).pow(1000).toString().chars();
		return Integer.toString(
				x
						//		.flatMapToInt(i->i) // note that this is needed since .char is an IntStream
						.map(Character::getNumericValue)
						.sum()
		);
	}


	/****************************************************************************
	 Problem 17:
	 """
	 If the numbers 1 to 5 are written out in words: one, two, three, four, five,
	 then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

	 If all the numbers from 1 to 1000 (one thousand) inclusive were written out in
	 words, how many letters would be used?

	 NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two)
	 contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of
	 "and" when writing out numbers is in compliance with British usage.
	 """
	 ****************************************************************************/
	@SuppressWarnings("unused")
	public static String euler17() {
		int Total = 0;
		for (int I = 1; I <= 1000; I++) {
			String Sstr = EulerUtils.english(I);
			Total += Sstr.length();
		}
		return Integer.toString(Total);
	}

	@SuppressWarnings("unused")
	public static String euler17b() {
		return Integer.toString(
				IntStream.rangeClosed(1, 1000)
						.map(i -> EulerUtils.english(i).length())
						.sum()
		);
	}


	/****************************************************************************
	 Problem 18:
	 """
	 By starting at the top of the triangle below and moving to adjacent
	 numbers on the row below, the maximum total from top to bottom is 23.

	 3
	 7 4
	 2 4 6
	 8 5 9 3

	 That is, 3 + 7 + 4 + 9 = 23.

	 Find the maximum total from top to bottom of the triangle below:

	 75
	 95 64
	 17 47 82
	 18 35 87 10
	 20 04 82 47 65
	 19 01 23 75 03 34
	 88 02 77 73 07 63 67
	 99 65 04 28 06 16 70 92
	 41 41 26 56 83 40 80 70 33
	 41 48 72 33 47 32 37 16 94 29
	 53 71 44 65 25 43 91 52 97 51 14
	 70 11 33 28 77 73 17 78 39 68 17 57
	 91 71 52 38 17 14 91 43 58 50 27 29 48
	 63 66 04 68 89 53 67 30 73 16 69 87 40 31
	 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

	 NOTE: As there are only 16384 routes, it is possible to solve this problem
	 by trying every route. However, Problem 67, is the same challenge with a
	 triangle containing one-hundred rows; it cannot be solved by brute force,
	 and requires a clever method! ;o)
	 """
	 ****************************************************************************/
	@SuppressWarnings("unused")
	public static String euler18() {

		int[][] triangle =
				{{75},
						{95, 64},
						{17, 47, 82},
						{18, 35, 87, 10},
						{20, 4, 82, 47, 65},
						{19, 1, 23, 75, 3, 34},
						{88, 2, 77, 73, 7, 63, 67},
						{99, 65, 4, 28, 6, 16, 70, 92},
						{41, 41, 26, 56, 83, 40, 80, 70, 33},
						{41, 48, 72, 33, 47, 32, 37, 16, 94, 29},
						{53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14},
						{70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57},
						{91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48},
						{63, 66, 4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31},
						{4, 62, 98, 27, 23, 9, 70, 98, 73, 93, 38, 53, 60, 4, 23}};

		int max_val = 0;
		max_val = euler18_rec(0, 0, triangle[0][0], triangle, max_val);
		return Integer.toString(max_val);

	}

	// Recursive function for euler18()
	public static int euler18_rec(int row, int col, int sum, int[][] tri, int max_val) {
		if (sum > max_val) {
			max_val = sum;
		}
		row++;
		if (row < tri.length) {
			for (int i = 0; i <= 1; i++) {
				max_val = euler18_rec(row, col + i, sum + tri[row][col + i], tri, max_val);
			}
		}
		return max_val;
	}


	/****************************************************************************
	 Problem 19:
	 """
	 You are given the following information, but you may prefer
	 to do some research for yourself.

	 * 1 Jan 1900 was a Monday.
	 * Thirty days has September,
	 April, June and November.
	 All the rest have thirty-one,
	 Saving February alone,
	 Which has twenty-eight, rain or shine.
	 And on leap years, twenty-nine.
	 * A leap year occurs on any year evenly divisible by 4, but not
	 on a century unless it is divisible by 400.

	 How many Sundays fell on the first of the month during the
	 twentieth century (1 Jan 1901 to 31 Dec 2000)?

	 """
	 ****************************************************************************/
	@SuppressWarnings("unused")
	public static String euler19() {
		int count = 0;
		for (int year = 1901; year <= 2000; year++) {
			for (int month = 1; month <= 12; month++) {
				if (LocalDate.of(year, month, 1).getDayOfWeek() == DayOfWeek.SUNDAY) {
					count++;
				}
			}
		}
		return Integer.toString(count);
	}

	@SuppressWarnings("unused")
	public static String euler19b() {
		return Integer.toString(
				IntStream.rangeClosed(1901, 2000)
						.map(year ->
								(int) IntStream.rangeClosed(1, 12)
										.filter(month -> LocalDate.of(year, month, 1).getDayOfWeek() == DayOfWeek.SUNDAY)
										.count()
						).sum()
		);
	}


	/****************************************************************************
	 Problem 20:
	 """
	 n! means n (n 1) ... 3 2 1

	 Find the sum of the digits in the number 100!")
	 """
	 ****************************************************************************/
	@SuppressWarnings("unused")
	public static String euler20() {
		String s = EulerUtils.factorialB(100).toString();
		return Integer.toString(EulerUtils.sum_string(s));
	}


	@SuppressWarnings("unused")
	public static String euler20b() {
		return Integer.toString(
				EulerUtils.factorialB(100).toString()
						.chars()
						.map(Character::getNumericValue)
						.sum()
		);
	}


	/****************************************************************************
	 Problem 21:
	 """
	 Let d(n) be defined as the sum of proper divisors of n (numbers less
	 than n which divide evenly into n).
	 If d(a) = b and d(b) = a, where a /= b, then a and b are an amicable
	 pair and each of a and b are called amicable numbers.

	 For example, the proper divisors of 220 are
	 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284.
	 The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

	 Evaluate the sum of all the amicable numbers under 10000.
	 """
	 ****************************************************************************/
	@SuppressWarnings("unused")
	public static String euler21() {
		Map<Integer, Integer> s = new HashMap<>();
		for (int a = 1; a <= 9999; a++) {
			int b = EulerUtils.sum_divisors(a);
			int c = EulerUtils.sum_divisors(b);
			if (a != b && a == c) {
				s.put(a, 1);
				s.put(b, 1);
			}
		}
		int sum = 0;
		for (int k : s.keySet()) {
			sum += k;
		}
		return Integer.toString(sum);
	}


	@SuppressWarnings("unused")
	public static String euler21b() {
		Map<Integer, Integer> s = new HashMap<>();
		IntStream.rangeClosed(1,9999)
				.forEach(
						a-> {
								int b = EulerUtils.sum_divisors(a);
								int c = EulerUtils.sum_divisors(b);
								if (a != b && a == c) {
									s.put(a, 1);
									s.put(b, 1);
								}
						}
				);

		int sum = 0;
		for (int k : s.keySet()) {
			sum += k;
		}
		return Integer.toString(sum);
	}


	@SuppressWarnings("unused")
	public static String euler21c() {
		Map<Integer, Integer> s = new HashMap<>();
		int sum = IntStream.rangeClosed(1,9999)
				.flatMap(a-> e21c(a))
				.distinct()
				.sum();
		return Integer.toString(sum);
	}

	@SuppressWarnings("unused")
	public static String euler21d() {
		int sum = IntStream.rangeClosed(1,9999)
				.flatMap(a-> {
								IntStream s = IntStream.of(0);
								int b = EulerUtils.sum_divisors(a);
								if (a != b && a == EulerUtils.sum_divisors(b)) {
									s = IntStream.of(a,b);
								}
								return s;
							}
				)
				.distinct()
				.sum();


		return Integer.toString(sum);

	}


	public static IntStream e21c(int a) {
		int b = EulerUtils.sum_divisors(a);
		int c = EulerUtils.sum_divisors(b);
		// This is a hack but it works since we do a distinct and sum...
		IntStream s = IntStream.of(0);
		if (a != b && a == c) {
			s = IntStream.of(a,b);
		}
		return s;
	}



	/****************************************************************************
	 Problem 22:
	 """
	 Using names.txt (right click and 'Save Link/Target As...'), a 46K
	 text file containing over five-thousand first names, begin by sorting
	 it into alphabetical order. Then working out the alphabetical value
	 for each name, multiply this value by its alphabetical position in the
	 list to obtain a name score.

	 For example, when the list is sorted into alphabetical order, COLIN,
	 which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in
	 the list. So, COLIN would obtain a score of 938 53 = 49714.

	 What is the total of all the name scores in the file?")
	 """
	 ****************************************************************************/
	@SuppressWarnings("unused")
	public static String euler22() {
		try {
			String file = "names.txt";
			String[] ss = Files.readAllLines(Paths.get(file)).get(0)
					.replaceAll("\"", "").split(",");
			Arrays.sort(ss); // inline sort
			int sum = 0;
			for (int i = 0; i < ss.length; i++) {
				sum += (i + 1) * EulerUtils.sum_alpha_pos(ss[i]);
			}
			return Integer.toString(sum);

		} catch (Exception e) {
			e.printStackTrace();
		}

		return "UNKNOWN";
	}

	// Stream version, with - as usual - a bit of cheating...
	// Slightly faster than euler22() but a little longer.
	@SuppressWarnings("unused")
	public static String euler22b() {

		try {
			String file = "names.txt";
			Stream<String> stream = Files.lines(Paths.get(file));
			Map<String, Integer> map = new HashMap<>();
			map.put("count", 1); // Cheating: This is an effectively final global variable. :-)
			return Integer.toString(
					stream
							.map(line -> line.replaceAll("\"", "").split(","))
							.flatMap(Arrays::stream) // flatten all words
							.sorted()
							.mapToInt(s -> {
										int count = map.get("count");
										map.put("count", count + 1);
										return count * EulerUtils.sum_alpha_pos(s);
									}
							)
							.sum()
			);

		} catch (Exception e) {
			e.printStackTrace();
		}
		return "UNKNOWN";
	}

	/****************************************************************************
	 Problem 23:
	 """
	 A perfect number is a number for which the sum of its proper divisors
	 is exactly equal to the number. For example, the sum of the proper divisors
	 of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

	 A number n is called deficient if the sum of its proper divisors is less than
	 n and it is called abundant if this sum exceeds n.

	 As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number
	 that can be written as the sum of two abundant numbers is 24. By mathematical
	 analysis, it can be shown that all integers greater than 28123 can be written
	 as the sum of two abundant numbers. However, this upper limit cannot be reduced
	 any further by analysis even though it is known that the greatest number that
	 cannot be expressed as the sum of two abundant numbers is less than this limit.

	 Find the sum of all the positive integers which cannot be written as the sum of
	 two abundant numbers.
	 """
	 ****************************************************************************/
	@SuppressWarnings("unused")
	public static String euler23() {
		// Note: This is a solution ported from Picat code which in turn is from C++(?) code,
		// Picat is 1-based so range 1..limit.
		int limit = 20161;
		int[] arr = new int[limit + 1];
		for (int i = 0; i <= limit; i++) {
			arr[i] = 1;
		}

		for (int i = 2; i < limit + 1; i++) {
			for (int j = i * 2; j <= limit; j = j + i) {
				arr[j] = arr[j] + i;
			}
		}
		List<Integer> abundant = new ArrayList<>();
		for (int i = 12; i <= limit; i++) {
			if (arr[i] > i) {
				abundant.add(i);
			}
		}
		for (int a : abundant) {
			for (int b : abundant) {
				if (b > a || a + b >= limit) {
					break;
				} else {
					arr[a + b] = 0;
				}
			}
		}
		int sum = 0;
		for (int i = 1; i <= limit; i++) {
			if (arr[i] != 0) {
				sum += i;
			}
		}
		return Integer.toString(sum);

	}


	/****************************************************************************
	 Problem 24:
	 """
	 A permutation is an ordered arrangement of objects. For example, 3124 is one
	 possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are
	 listed numerically or alphabetically, we call it lexicographic order. The
	 lexicographic permutations of 0, 1 and 2 are:

	 012   021   102   120   201   210

	 What is the millionth lexicographic permutation of the digits
	 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
	 """
	 ****************************************************************************/
	// Much faster
	@SuppressWarnings("unused")
	public static String euler24() {
		int[] p = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
		int c = 1;
		while (c < 1_000_000) {
			p = EulerUtils.next_permutation(p);
			c++;
		}
		return Arrays.toString(p);
	}

	// Inspired by a program at 'net (via some transformations)
	// 0.000097s
	@SuppressWarnings("unused")
	public static String euler24b() {
		int n = 999_999;
		int p = 10;
		List<Integer> eli = new ArrayList<>();
		for (int i = 1; i <= p; i++) {
			eli.add(i % 10);
		}
		StringBuilder answer = new StringBuilder();
		for (int i = 1; i < p; i++) {
			int f = EulerUtils.factorial(p - i);
			int d = n / f;
			n %= f;
			int dd = eli.get(d - 1);
			answer.append(dd);
			eli.remove(new Integer(dd));
		}
		answer.append(eli.get(0));
		return answer.toString();

	}


	// takes 1.06s
	@SuppressWarnings("unused")
	public static String euler24c() {
		List<String> strings = EulerUtils.permutation("0123456789");
		return strings.get(1_000_000 - 1);
	}


	/****************************************************************************
	 Problem 25:
	 """
	 The Fibonacci sequence is defined by the recurrence relation:

	 Fn = Fn1 + Fn2, where F1 = 1 and F2 = 1.

	 Hence the first 12 terms will be:

	 F1 = 1
	 F2 = 1
	 F3 = 2
	 F4 = 3
	 F5 = 5
	 F6 = 8
	 F7 = 13
	 F8 = 21
	 F9 = 34
	 F10 = 55
	 F11 = 89
	 F12 = 144

	 The 12th term, F12, is the first term to contain three digits.

	 What is the first term in the Fibonacci sequence to contain 1000 digits?")
	 """
	 ****************************************************************************/
	// 0.049s
	@SuppressWarnings("unused")
	public static String euler25() {
		int target = 1_000;
		int foundUpper = 0;
		int i = 1;
		int fibLen = 0;
		int step = 143;
		// find a good range of where to look
		// TODO: do a binary search instead
		while (fibLen < target && foundUpper == 0) {
			fibLen = EulerUtils.fibABig(step * i).toString().length();
			if (fibLen > target) {
				foundUpper = i;
			}
			i++;
		}

		int fib = step * (foundUpper - 1);
		fibLen = EulerUtils.fibABig(fib).toString().length();
		while (fibLen < target && fib <= step * foundUpper) {
			fibLen = EulerUtils.fibABig(fib).toString().length();
			if (fibLen < target) {
				fib++;
			}
		}
		// return "fib: " + fib + " fibLen: " + fibLen;
		return Integer.toString(fib);
	}


	// 0.99s
	@SuppressWarnings("unused")
	public static String euler25b() {
		BigInteger fib;

		int len = 0;
		int i = 2;
		while (len < 1_000) {
			fib = EulerUtils.fibABig(i);
			len = fib.toString().length();
			// System.out.println("i: " + i + "fib: " + fib + " len: " + len);
			i++;
		}

		return Integer.toString(i - 1);
	}

	// 0.018s
	@SuppressWarnings("unused")
	public static String euler25c() {
		int target = 1_000;
		int value = e25_binary_search(target);
		// int value = e25_binary_search(target);
		// here we get one value which has 1000 digits
		// but perhaps not the first so we must check backward
		while (EulerUtils.fib_length(value - 1) >= target) {
			value--;
		}
		return Integer.toString(value);
	}


	// "binary search" approach for euler25
	// TODO: add the method to work with
	public static int e25_binary_search(int target) {
		// public static int e25_binary_search(int target) {
		int v = 0;
		int low = 0;
		int high = target;
		int mid;
		boolean found = false;
		while (!found) {
			mid = (low + high) / 2;
			int value = EulerUtils.fib_length(mid);
			// int value = f.call(mid);
			if (low > mid) {
				high = high * 2; // high is too low: increment it
			} else if (value > target) {
				high = mid - 1;
			} else if (value < target) {
				low = mid + 1;
			} else {
				v = mid;
				found = true;
			}
		}
		return v;
	}


	/****************************************************************************
	 Problem 26:
	 """
	 A unit fraction contains 1 in the numerator. The decimal representation of the
	 unit fractions with denominators 2 to 10 are given:

	 1/2	= 	0.5
	 1/3	= 	0.(3)
	 1/4	= 	0.25
	 1/5	= 	0.2
	 1/6	= 	0.1(6)
	 1/7	= 	0.(142857)
	 1/8	= 	0.125
	 1/9	= 	0.(1)
	 1/10	= 	0.1

	 Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
	 seen that 1/7 has a 6-digit recurring cycle.

	 Find the value of d < 1000 for which 1/d contains the longest recurring cycle in
	 its decimal fraction part.
	 """
	 ****************************************************************************/
	// 0.004
	@SuppressWarnings("unused")
	public static String euler26() {
		int max_len = 0;
		int max_d = 0;
		for (int d = 2; d <= 999; d++) {
			if (EulerUtils.is_prime_simple(d)) {
				int len = EulerUtils.get_rep_len(d);
				if (len > max_len) {
					max_len = len;
					max_d = d;
				}
			}
		}
		return Integer.toString(max_d);
	}

	// 0.096s
	@SuppressWarnings("unused")
	public static String euler26b() {
		Map<String, Integer> map = new HashMap<>();
		map.put("max_len", 0);
		map.put("max_d", 0);
		IntStream.rangeClosed(2, 999)
				.forEach(i -> {
					int len = EulerUtils.get_rep_len(i);
					if (len > map.get("max_len")) {
						map.put("max_len", len);
						map.put("max_d", i);
					}
				});
		return Integer.toString(map.get("max_d"));
	}


	/****************************************************************************
	 Problem 27:
	 """
	 Euler published the remarkable quadratic formula:

	 n^2 + n + 41

	 It turns out that the formula will produce 40 primes for the consecutive values
	 n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by
	 41, and certainly when n = 41, 41^2 + 41 + 41 is clearly divisible by 41.

	 Using computers, the incredible formula  n^2 − 79n + 1601 was discovered, which
	 produces 80 primes for the consecutive values n = 0 to 79. The product of the
	 coefficients, −79 and 1601, is −126479.

	 Considering quadratics of the form:

	 n^2 + an + b, where |a| < 1000 and |b| < 1000

	 where |n| is the modulus/absolute value of n
	 e.g. |11| = 11 and |−4| = 4

	 Find the product of the coefficients, a and b, for the quadratic
	 expression that produces the maximum number of primes for consecutive
	 values of n, starting with n = 0.
	 """
	 ****************************************************************************/
	// 0.12s
	@SuppressWarnings("unused")
	public static String euler27() {
		int t = 999;
		int bestLen = 0;
		int bestA = 0;
		int bestB = 0;
		for (int a = -t; a <= t; a++) {
			for (int b = -t; b <= t; b++) {
				int len = 0;
				int pp = len * len + a * len + b;
				while (pp > 1 && EulerUtils.is_prime_simple(pp)) {
					len++;
					pp = len * len + a * len + b;
				}
				if (len > bestLen) {
					bestLen = len;
					bestA = a;
					bestB = b;
				}
			}
		}
		return Integer.toString(bestA * bestB);
	}


	/****************************************************************************
	 Problem 28:
	 """
	 Starting with the number 1 and moving to the right in a clockwise
	 direction a 5 by 5 spiral is formed as follows:

	 21 22 23 24 25
	 20  7  8  9 10
	 19  6  1  2 11
	 18  5  4  3 12
	 17 16 15 14 13

	 It can be verified that the sum of the numbers on the diagonals is 101.

	 What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
	 """
	 ****************************************************************************/
	// 0.00014s
	@SuppressWarnings("unused")
	public static String euler28() {
		int s = 1;
		int n = 3;
		while (n <= 1001) {
			s += 4 * n * n - 6 * n + 6;
			n += 2;
		}

		return Integer.toString(s);
	}

	// 0.005s
	@SuppressWarnings("unused")
	public static String euler28b() {
		return Integer.toString(
				// The leading 1 is since we start at 1
				1 +
						IntStream.rangeClosed(3, 1001)
								.filter(n -> n % 2 == 1)
								.map(n -> 4 * n * n - 6 * n + 6)
								.sum()

		);
	}

	@SuppressWarnings("unused")
	public static String euler28c() {
		int s = 1;
		for (int n : EulerUtils.range_array(3, 1001, 2)) {
			s += 4 * n * n - 6 * n + 6;
		}

		return Integer.toString(s);
	}


	/****************************************************************************
	 Problem 29:
	 """
	 Consider all integer combinations of a^b for 2 <= a <= 5 and 2 <= b <= 5:

	 2^2=4, 2^3=8, 2^4=16, 2^5=32
	 3^2=9, 3^3=27, 3^4=81, 3^5=243
	 4^2=16, 4^3=64, 4^4=256, 4^5=1024
	 5^2=25, 5^3=125, 5^4=625, 5^5=3125

	 If they are then placed in numerical order, with any repeats removed, we get the
	 following sequence of 15 distinct terms:

	 4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125

	 How many distinct terms are in the sequence generated by a^b for
	 2 <= a <= 100 and 2 <= b <= 100?

	 """
	 ****************************************************************************/
	// 0.0089s
	@SuppressWarnings("unused")
	public static String euler29() {
		int min = 2;
		int max = 100;
		Map<Double, Integer> map = new HashMap<>();
		for (int a = min; a <= max; a++) {
			for (int b = min; b <= max; b++) {
				map.put(Math.pow(a, b), 1);
			}
		}
		return Integer.toString(map.size());
	}

	// 0.0030
	@SuppressWarnings("unused")
	public static String euler29b() {
		int min = 2;
		int max = 100;
		Set<Double> set = new HashSet<>();
		for (int a = min; a <= max; a++) {
			for (int b = min; b <= max; b++) {
				set.add(Math.pow(a, b));
			}
		}
		return Integer.toString(set.size());
	}


	/****************************************************************************
	 Problem 30:
	 """
	 Surprisingly there are only three numbers that can be written
	 as the sum of fourth powers of their digits:

	 1634 = 1^(4) + 6^(4) + 3^(4) + 4^(4)
	 8208 = 8^(4) + 2^(4) + 0^(4) + 8^(4)
	 9474 = 9^(4) + 4^(4) + 7^(4) + 4^(4)

	 As 1 = 1^(4) is not a sum it is not included.

	 The sum of these numbers is 1634 + 8208 + 9474 = 19316.

	 Find the sum of all the numbers that can be written as the sum of
	 fifth powers of their digits.
	 """
	 ****************************************************************************/
	// 0.0089s
	@SuppressWarnings("unused")
	public static String euler30() {
		int sum = 0;
		int n = 10;
		int t = 354294; // 6*9**5
		while (n < t) {
			sum += n == EulerUtils.sum_string_digits_pow(n, 5) ? n : 0;
			n++;
		}
		return Integer.toString(sum);
	}

	@SuppressWarnings("unused")
	public static String euler30b() {
		int sum = 0;
		// 354294 = 6*9**5
		for (int n : EulerUtils.range_array(10, 354294)) {
			sum += n == EulerUtils.sum_string_digits_pow(n, 5) ? n : 0;
			n++;
		}
		return Integer.toString(sum);
	}


	/****************************************************************************
	 Problem 31:
	 """
	 In England the currency is made up of pound, £, and pence, p, and
	 there are eight coins in general circulation:

	 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

	 It is possible to make £2 in the following way:

	 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

	 How many different ways can £2 be made using any number of coins?
	 """
	 ****************************************************************************/
	// 0.011763 (uncached)
	// TODO: Check how to memoize with arrays as parameter.
	@SuppressWarnings("unused")
	public static String euler31() {
		int[] coins = {200, 100, 50, 20, 10, 5, 2, 1};

		int t = e31_coins(coins, 200, 0);
		return Integer.toString(t);

	}

	public static int e31_coins(int[] coins, int money, int m) {
		int sum = 0;
		int len = coins.length;
		if (m == len) {
			sum = 1;
		} else {
			for (int i = m; i < len; i++) {
				if (money - coins[i] == 0) {
					sum++;
				} else {
					if (money - coins[i] > 0) {
						sum += e31_coins(coins, money - coins[i], i);
					}
				}
			}
		}

		return sum;

	}


	/****************************************************************************
	 Problem 32:
	 """
	 We shall say that an n-digit number is pandigital if it makes use of
	 all the digits 1 to n exactly once; for example, the 5-digit number,
	 15234, is 1 through 5 pandigital.

	 The product 7254 is unusual, as the identity, 39 × 186 = 7254,
	 containing multiplicand, multiplier, and product is 1 through 9
	 pandigital.

	 Find the sum of all products whose multiplicand/multiplier/product
	 identity can be written as a 1 through 9 pandigital.
	 HINT: Some products can be obtained in more than one way so be sure
	 to only include it once in your sum.
	 """
	 ****************************************************************************/
	// 0.1311s
	@SuppressWarnings("unused")
	public static String euler32() {
		int sum = 0;
		Map<Integer, Integer> prodHash = new HashMap<>();
		for (int a : EulerUtils.range_array(2, 98)) {
			for (int b : EulerUtils.range_array(a + 1, 9876)) {
				int prod = a * b;
				String s = "" + a + b + prod;
				if (s.length() == 9 && !s.contains("0")) {
					Map<Integer, Integer> hash = new HashMap<>();
					for (char c : s.toCharArray()) {
						hash.put(Character.getNumericValue(c), 1);
					}
					if (hash.keySet().size() == 9 && !prodHash.containsKey(prod)) {
						sum += prod;
						prodHash.put(prod, 1);
					}
				}
			}
		}
		return Integer.toString(sum);
	}

	// a little slower: 0.1514s
	@SuppressWarnings("unused")
	public static String euler32b() {
		int sum = 0;
		Map<Integer, Integer> prodHash = new HashMap<>();
		for (int a : EulerUtils.range_array(2, 98)) {
			for (int b : EulerUtils.range_array(a + 1, 9876)) {
				int prod = a * b;
				String s = "" + a + b + prod;
				if (s.length() == 9 && !s.contains("0")) {
					int slen = (int) s.chars()
							.distinct()
							.count();
					if (slen == 9 && !prodHash.containsKey(prod)) {
						sum += prod;
						prodHash.put(prod, 1);
					}
				}
			}
		}
		return Integer.toString(sum);
	}


	/****************************************************************************
	 Problem 33:
	 """
	 The fraction 49/98 is a curious fraction, as an inexperienced mathematician in
	 attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct,
	 is obtained by cancelling the 9s.

	 We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

	 There are exactly four non-trivial examples of this type of fraction, less than
	 one in value, and containing two digits in the numerator and denominator.

	 If the product of these four fractions is given in its lowest common terms, find
	 the value of the denominator.
	 """
	 ****************************************************************************/
	// 0.0002s
	@SuppressWarnings("unused")
	public static String euler33() {
		double s = 1;
		for (int y : EulerUtils.range_array(1, 9)) {
			// for(int z: EulerUtils.range_array(y,9)) {
			for (int z = y; z <= 9; z++) {
				double x = 9.0 * y * z / (10.0 * y - z);
				if (Math.floor(x) == x && y / z < 1.0 && x < 10.0) {
					s = (s * y) / z; // (s*y)/z;
				}
			}
		}
		return ("" + (int) Math.floor(1 / s));
	}


	/****************************************************************************
	 Problem 34:
	 """
	 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

	 Find the sum of all numbers which are equal to the sum of the
	 factorial of their digits.

	 Note: as 1! = 1 and 2! = 2 are not sums they are not included.
	 """
	 ****************************************************************************/
	// 0.0002s
	@SuppressWarnings("unused")
	public static String euler34() {
		int sum = 0;
		for (int n = 10; n <= 100_000; n++) {
			int s = 0;
			for (char c : Integer.toString(n).toCharArray()) {
				s += EulerUtils.factorial(Character.getNumericValue(c));
			}
			if (n == s) {
				sum += n;
			}
		}

		return Integer.toString(sum);
	}


	/****************************************************************************
	 Problem 35:
	 """
	 The number, 197, is called a circular prime because all rotations
	 of the digits: 197, 971, and 719, are themselves prime.

	 There are thirteen such primes below 100:
	 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

	 How many circular primes are there below one million?
	 """
	 ****************************************************************************/
	// 0.276s
	@SuppressWarnings("unused")
	public static String euler35() {
		Map<Integer, Integer> primeMap = new HashMap<>();
		primeMap.put(2, 1);
		for (int i = 3; i < 1_000_000; i += 2) {
			if (EulerUtils.is_prime_simple(i)) {
				primeMap.put(i, 1);
			}
		}
		int count = 0;
		for (Map.Entry<Integer, Integer> e : primeMap.entrySet()) {
			if (is_circular_prime(e.getKey(), primeMap)) {
				count++;
			}
		}
		return Integer.toString(count);
	}


	// For euler35
	public static boolean is_circular_prime(int p, Map<Integer, Integer> primeMap) {
		String s = Integer.toString(p);
		int v = p; // we know that this is a prime
		int len = s.length();
		for (int i = 1; i < len; i++) {
			v = Integer.valueOf(s.substring(i, len) + s.substring(0, i));
			if (!primeMap.containsKey(v)) {
				return false;
			}
		}
		return primeMap.containsKey(v);
	}


	/****************************************************************************
	 Problem 36:
	 """
	 The decimal number, 585 = 1001001001_(2) (binary), is palindromic
	 in both bases.

	 Find the sum of all numbers, less than one million, which are palindromic
	 in base 10 and base 2.

	 (Please note that the palindromic number, in either base, may not
	 include leading zeros.)
	 """
	 ****************************************************************************/
	//
	@SuppressWarnings("unused")
	public static String euler36() {
		int sum = 0;
		for (int i = 1; i <= 1_000_000; i++) {
			if (EulerUtils.isPalindrome(Integer.toString(i)) &&
					EulerUtils.isPalindrome(Integer.toString(i, 2))) {
				sum += i;
			}
		}
		return Integer.toString(sum);

	}

	@SuppressWarnings("unused")
	public static String euler36b() {

		return Integer.toString(
				IntStream.rangeClosed(1, 1_000_000)
						.filter(i -> EulerUtils.isPalindrome(Integer.toString(i)) &&
								EulerUtils.isPalindrome(Integer.toString(i, 2)))
						.sum()
		);
	}


	/****************************************************************************
	 Problem 37:
	 """
	 The number 3797 has an interesting property. Being prime itself, it is possible to
	 continuously remove digits from left to right, and remain prime at each stage:
	 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

	 Find the sum of the only eleven primes that are both truncatable from left to right
	 and right to left.

	 NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
	 """
	 ****************************************************************************/
	// 0.085s
	@SuppressWarnings("unused")
	public static String euler37() {
		// 2, 3, 5, and 7 are not considered truncable primes
		int p = 11;
		int sum = 0;
		int c = 0;
		while (c < 11) {
			if (e37_check(p) && EulerUtils.is_prime_simple(p)) {
				c++;
				sum += p;
			}
			p += 2;
		}

		return Integer.toString(sum);

	}


	public static boolean e37_check(int n) {
		String s = Integer.toString(n);
		int len = s.length();
		int tmp = n;
		for (int i = 1; i < len; i++) {
			tmp = Integer.valueOf(s.substring(i, len));
			if (!EulerUtils.is_prime_simple(tmp)) {
				return false;
			}
			tmp = Integer.valueOf(s.substring(0, i));
			if (!EulerUtils.is_prime_simple(tmp)) {
				return false;
			}
		}
		return EulerUtils.is_prime_simple(tmp);
	}


	/****************************************************************************
	 Problem 38:
	 """
	 Take the number 192 and multiply it by each of 1, 2, and 3:

	 192 × 1 = 192
	 192 × 2 = 384
	 192 × 3 = 576

	 By concatenating each product we get the 1 to 9 pandigital,
	 192384576. We will call 192384576 the concatenated product of 192
	 and (1,2,3)

	 The same can be achieved by starting with 9 and multiplying by
	 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the
	 concatenated product of 9 and (1,2,3,4,5).

	 What is the largest 1 to 9 pandigital 9-digit number that can be
	 formed as the concatenated product of an integer with
	 (1,2, ... , n) where n > 1?
	 """
	 ****************************************************************************/
	// 0.018s
	@SuppressWarnings("unused")
	public static String euler38() {
		String maxN = "";
		for (int n = 9876; n >= 9; n--) {
			if (!Objects.equals(maxN, "")) {
				// We got a number (maxN)!
				break;
			}
			String s = Integer.toString(n);
			int i = 2;
			while (s.length() < 9) {
				s += Integer.toString(n * i);
				i++;
			}
			// int sLen = s.length();
			if (is_pandigit(s, false)) {
				maxN = s;
			}
		}
		return maxN;
	}


	@SuppressWarnings("unused")
	public static String euler38b() {
		String maxN = "";
		int n = 9876;
		// using a while loop instead
		while (Objects.equals(maxN, "")) {
			int i = 2;
			String s = Integer.toString(n);
			while (s.length() < 9) {
				s += Integer.toString(n * i);
				i++;
			}
			if (is_pandigit(s, false)) {
				maxN = s;
			}
			n--;

		}
		return maxN;
	}

	// Check is n is a pandigit number:
	// - include_0 = true: includes a 0 and is 10 digits
	// - include_0 = false: don't include a 0 and is 9 digits
	public static boolean is_pandigit(String s, boolean include_0) {
		// String s = Integer.toString(n);
		int target_len = include_0 ? 10 : 9;
		if (s.length() == target_len && (include_0 || !s.contains("0"))) {
			int slen = (int) s.chars()
					.distinct()
					.count();
			if (slen == target_len) {
				return true;
			}
		}
		return false;

	}


	/****************************************************************************
	 Problem 39:
	 """
	 If p is the perimeter of a right angle triangle with integral length sides,
	 {a,b,c}, there are exactly three solutions for p = 120.

	 {20,48,52}, {24,45,51}, {30,40,50}

	 For which value of p <= 1000, is the number of solutions maximised?
	 """
	 ****************************************************************************/
	// 0.054s
	@SuppressWarnings("unused")
	public static String euler39() {
		int n = 1000 - 1;
		Map<Integer, Integer> squares = new HashMap<>();
		for (int i = 1; i <= n; i++) {
			squares.put(i * i, 1);
		}
		Map<Integer, Integer> counts = new HashMap<>();
		for (Map.Entry<Integer, Integer> xe : squares.entrySet()) {
			for (Map.Entry<Integer, Integer> ye : squares.entrySet()) {
				int x = xe.getKey();
				int y = ye.getKey();
				if (x < y &&
						(Math.sqrt(x) + Math.sqrt(y) + Math.sqrt(x + y) < 1000) &&
						squares.containsKey(x + y)) {
					int c = (int) (Math.sqrt(x) + Math.sqrt(y) + Math.sqrt(x + y));
					counts.put(c, counts.getOrDefault(c, 0) + 1);
				}
			}
		}
		int maxValue = 0;
		int maxN = 0;
		for (Map.Entry<Integer, Integer> c : counts.entrySet()) {
			if (c.getValue() > maxValue) {
				maxValue = c.getValue();
				maxN = c.getKey();
			}
		}
		return Integer.toString(maxN);
	}


	/****************************************************************************
	 Problem 40:
	 """
	 An irrational decimal fraction is created by concatenating the positive integers:

	 0.123456789101112131415161718192021...

	 It can be seen that the 12th digit of the fractional part is 1.

	 If dn represents the nth digit of the fractional part, find the
	 value of the following expression.

	 d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
	 """
	 ****************************************************************************/
	// 0.0145s
	@SuppressWarnings("unused")
	public static String euler40() {
		int i = 1;
		int dlen = 1;
		int prod = 1;
		int index = 10;
		while (dlen < 1_000_000) {
			i++;
			String istr = Integer.toString(i);
			int len = istr.length();
			if (dlen + len >= index) {
				prod = prod * Character.getNumericValue(istr.charAt(index - dlen - 1));
				index *= 10;
			}
			dlen += len;
		}
		return Integer.toString(prod);
	}


	/****************************************************************************
	 Problem 40:
	 """
	 We shall say that an n-digit number is pandigital if it makes use of all
	 the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital
	 and is also prime.

	 What is the largest n-digit pandigital prime that exists?
	 """
	 ****************************************************************************/
	// 0.0103s
	// If we start at n=9: 0231s
	@SuppressWarnings("unused")
	public static String euler41() {

		// Simplification:
		// n=9 is not possible since 1+2+3+4+5+6+7+8+9=45 is divisible by 3
		// n=8 is not possible since 1+2+3+4+5+6+7+8=36 is divisible by 3
		int n = 7;
		int m = 0;
		int v;
		String ps;
		// Note: we cannot use next_permutation/1 since we start at the
		//       last permutation (i.e. the revered order)
		while (m == 0 && n >= 4) {
			ps = "";
			for (int i = 0; i < n; i++) {
				ps += Integer.toString(n - i);
			}
			// Note: we rely on the order of permutations here
			// It is reversed ordered...
			List<String> perms = EulerUtils.all_permutations(ps);
			for (String p : perms) {
				v = Integer.valueOf(p);
				if (EulerUtils.is_prime_simple(v)) {
					m = v; // found it!
					break;
				}
			}
			n--;
		}
		return Integer.toString(m);
	}


	/****************************************************************************
	 Problem 42:
	 """
	 The nth term of the sequence of triangle numbers is given by,
	 tn = 1/2*n*(n+1);
	 so the first ten triangle numbers are:

	 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

	 By converting each letter in a word to a number corresponding to its
	 alphabetical position and adding these values we form a word value. For example,
	 the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value
	 is a triangle number then we shall call the word a triangle word.

	 Using words.txt (right click and 'Save Link/Target As...'), a 16K text file
	 containing nearly two-thousand common English words, how many
	 are triangle words?
	 """
	 ****************************************************************************/
	// 0.0103s
	@SuppressWarnings("unused")
	public static String euler42() {
		String file = "words.txt";
		try {
			String[] ss = Files.readAllLines(Paths.get(file)).get(0)
					.replaceAll("\"", "").split(",");
			Arrays.sort(ss); // inline sort
			Map<Integer, Integer> map = new HashMap<>();
			for (int i = 1; i <= 20; i++) {
				map.put(EulerUtils.triangle_number(i), 1);
			}
			int c = 0;
			for (String s : ss) {
				if (map.containsKey(EulerUtils.e42_get_score(s))) {
					c++;
				}
			}
			return Integer.toString(c);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return "UNKNOWN";
	}

	@SuppressWarnings("unused")
	public static String euler42b() {
		String file = "words.txt";
		try {
			String[] ss = Files.readAllLines(Paths.get(file)).get(0)
					.replaceAll("\"", "").split(",");
			Arrays.sort(ss); // inline sort
			Map<Integer, Integer> map = new HashMap<>();
			for (int i = 1; i <= 20; i++) {
				map.put(EulerUtils.triangle_number(i), 1);
			}

			Stream<String> stream = Stream.of(ss);
			return Integer.toString((int)
					stream
							.filter(s -> map.containsKey(EulerUtils.e42_get_score(s)))
							.count());

		} catch (Exception e) {
			e.printStackTrace();
		}
		return "UNKNOWN";
	}


	/****************************************************************************
	 Problem 43:
	 """
	 The number, 1406357289, is a 0 to 9 pandigital number because it is made up of
	 each of the digits 0 to 9 in some order, but it also has a rather interesting
	 sub-string divisibility property.

	 Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we
	 note the following:

	 * d2d3d4=406 is divisible by 2
	 * d3d4d5=063 is divisible by 3
	 * d4d5d6=635 is divisible by 5
	 * d5d6d7=357 is divisible by 7
	 * d6d7d8=572 is divisible by 11
	 * d7d8d9=728 is divisible by 13
	 * d8d9d10=289 is divisible by 17

	 Find the sum of all 0 to 9 pandigital numbers with this property.
	 """
	 ****************************************************************************/
	// 1.322s!!!
	@SuppressWarnings("unused")
	public static String euler43() {
		int[] pp = {2, 3, 5, 7, 11, 13, 17};
		// Using a string to represent the number so we don't have to
		// convert BigInteger -> String -> chars and back
		// (also: I didn't get the all_permutations for BigInteger to work)
		String s = "1023456789";
		List<String> perms = EulerUtils.all_permutations(s);
		BigInteger sum = BigInteger.ZERO;
		int c = 0;
		for (String p : perms) {
			boolean check = true;
			for (int i = 0; i < 7; i++) {
			// for (int i = 6; i >= 0; i--) {
				if ((100 * Character.getNumericValue(p.charAt(i + 1)) +
					  10 * Character.getNumericValue(p.charAt(i + 2)) +
					       Character.getNumericValue(p.charAt(i + 3)))
						 % pp[i] != 0
						) {
					check = false;
					break;
				}
			}
			if (check) {
				// System.out.println("p " + p);
				sum = sum.add(new BigInteger(p));
			}
		}

		return sum.toString();
	}

	// Using map of char -> integer
	// Much slower: 1.767s
	@SuppressWarnings("unused")
	public static String euler43b() {
		int[] pp = {2, 3, 5, 7, 11, 13, 17};
		String s = "1023456789";
		// System.out.println(s);
		List<String> perms = EulerUtils.all_permutations(s);
		BigInteger sum = BigInteger.ZERO;
		Map<Character,Integer> chars = new HashMap<>();
		chars.put('0',0);
		chars.put('1',1);
		chars.put('2',2);
		chars.put('3',3);
		chars.put('4',4);
		chars.put('5',5);
		chars.put('6',6);
		chars.put('7',7);
		chars.put('8',8);
		chars.put('9',9);
		int c = 0;
		for (String p : perms) {
			boolean check = true;
			for (int i = 0; i < 7; i++) {
				if ((100 * chars.get(p.charAt(i + 1)) +
						10 * chars.get(p.charAt(i + 2)) +
						chars.get(p.charAt(i + 3)))
						% pp[i] != 0
						) {
					check = false;
					break;
				}
			}
			if (check) {
				// System.out.println("p " + p);
				sum = sum.add(new BigInteger(p));
			}
		}

		return sum.toString();
	}




	/****************************************************************************
	 Problem 44:
	 """
	 Pentagonal numbers are generated by the formula, P(n)=n(3n−1)/2.
	 The first ten pentagonal numbers are:

	 1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...

	 It can be seen that P(4) + P(7) = 22 + 70 = 92 = P(8). However,
	 their difference,  70 − 22 = 48, is not pentagonal.

	 Find the pair of pentagonal numbers, P(j) and P(k), for which their sum
	 and difference is pentagonal and D = |P(k) − P(j)| is minimised; what
	 is the value of D?

	 """
	 ****************************************************************************/
	// 0.072s
	@SuppressWarnings("unused")
	public static String euler44() {
		Set<Integer> set = new HashSet<>();
		int n = 2500;
		int[] s = new int[n];
		for (int i = 1; i <= n; i++) {
			s[i - 1] = i * (3 * i - 1) / 2;
			set.add(s[i - 1]);
		}
		int d = 10_000_000;
		for (int i = 0; i < n; i++) {
			for (int i2 = 0; i2 < n; i2++) {
				int j = s[n - i - 1];
				int k = s[i2];
				int a = j + k;
				int b = Math.abs(j - k);
				if (j < k &&
						a < d &&
						set.contains(a) &&
						b < d &&
						set.contains(b)
						) {
					d = b;
				}
			}
		}
		return Integer.toString(d);

	}

	/****************************************************************************
	 Problem 45:
	 """
	 Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:

	 Triangle 	  	Tn=n(n+1)/2 	  	1, 3, 6, 10, 15, ...
	 Pentagonal 	  	Pn=n(3n−1)/2 	  	1, 5, 12, 22, 35, ...
	 Hexagonal 	  	Hn=n(2n−1) 	  	1, 6, 15, 28, 45, ...

	 It can be verified that T(285) = P(165) = H(143) = 40755.

	 Find the next triangle number that is also pentagonal and hexagonal.
	 """
	 ****************************************************************************/
	// 0.045s
	@SuppressWarnings("unused")
	public static String euler45() {
		BigInteger t = BigInteger.valueOf(285 + 1);
		BigInteger tt = triB(t);
		BigInteger p = BigInteger.valueOf(165);
		BigInteger pp = pentB(p);
		BigInteger h = BigInteger.valueOf(143);
		BigInteger hh = hexB(h);
		while (tt.compareTo(pp) != 0 || pp.compareTo(hh) != 0) {
			t = t.add(BigInteger.ONE);
			tt = triB(t);
			if (tt.compareTo(pp) == 1) {
				p = p.add(BigInteger.ONE);
				pp = pentB(p);
			}
			if (pp.compareTo(hh) == 1) {
				h = h.add(BigInteger.ONE);
				hh = hexB(h);
			}
			if (tt.compareTo(hh) == 1) {
				h = h.add(BigInteger.ONE);
				hh = hexB(h);
			}
		}
		return tt.toString();
	}

	public static BigInteger triB(BigInteger n) {
		// n*(n+1) /2;
		return (n.multiply(n.add(BigInteger.ONE))).divide(BigInteger.valueOf(2));
	}

	public static BigInteger pentB(BigInteger n) {
		// n*(3*n-1) /2;
		return (n.multiply((n.multiply(BigInteger.valueOf(3)).subtract(BigInteger.ONE)))).divide(BigInteger.valueOf(2));
	}

	public static BigInteger hexB(BigInteger n) {
		// n*(2*n-1)
		return n.multiply(n.multiply(BigInteger.valueOf(2)).subtract(BigInteger.ONE));
	}


	/****************************************************************************
	 Problem 46:
	 """
	 It was proposed by Christian Goldbach that every odd composite number can be
	 written as the sum of a prime and twice a square.

	 9 = 7 + 2×1^2
	 15 = 7 + 2×2^2
	 21 = 3 + 2×3^2
	 25 = 7 + 2×3^2
	 27 = 19 + 2×2^2
	 33 = 31 + 2×1^2

	 It turns out that the conjecture was false.

	 What is the smallest odd composite that cannot be written as the
	 sum of a prime and twice a square?
	 """
	 ****************************************************************************/
	// 0.0115s
	@SuppressWarnings("unused")
	public static String euler46() {
		int res = 0;
		boolean gotIt = false;
		for (int i = 3; i <= 10_000; i += 2) {
			if (EulerUtils.is_prime_simple(i)) {
				continue;
			}
			int s = (int) Math.sqrt((double) i / 2);
			boolean found = false;
			for (int j = 1; j <= s; j++) {
				int ts = j * j * 2;
				if (EulerUtils.is_prime_simple(Math.abs(i - ts))) {
					found = true;
				}
			}
			if (!found) {
				res = i;
				break;
			}
		}
		return Integer.toString(res);
	}


	/****************************************************************************
	 Problem 47:
	 """
	 The first two consecutive numbers to have two distinct prime factors are:

	 14 = 2 x 7
	 15 = 3 x 5

	 The first three consecutive numbers to have three distinct
	 prime factors are:

	 644 = 2^2 x 7 x 23
	 645 = 3 x 5 x 43
	 646 = 2 x 17 x 19.

	 Find the first four consecutive integers to have four distinct primes
	 factors. What is the first of these numbers?
	 """
	 ****************************************************************************/
	// 0.015s
	@SuppressWarnings("unused")
	public static String euler47() {
		int maxN = 1_000_000;
		int[] f = new int[maxN + 1];
		String result = "";
		for (int i = 0; i <= maxN; i++) {
			f[i] = 0;
		}
		for (int i = 2; i <= maxN - 1; i++) {
			if (f[i] == 0) {
				for (int j = 2 * i; j <= maxN - 1; j += i) {
					f[j]++;
				}
			}
		}
		for (int i = 2; i <= maxN - 3; i++) {
			boolean found = true;
			for (int j = i; j <= i + 3; j++) {
				if (f[j] != 4) {
					found = false;
				}
			}
			if (found) {
				result = Integer.toString(i);
				break;
			}

		}
		return result;
	}


	/****************************************************************************
	 Problem 48:
	 """
	 The series, 1^(1) + 2^(2) + 3^(3) + ... + 10^(10) = 10405071317.

	 Find the last ten digits of the series,
	 1^(1) + 2^(2) + 3^(3) + ... + 1000^(1000).
	 """
	 ****************************************************************************/
	// 0.114s
	@SuppressWarnings("unused")
	public static String euler48() {
		BigInteger sum = BigInteger.ZERO;
		BigInteger t = new BigInteger("10000000000");

		for (int i = 1; i < 1_000; i++) {
			BigInteger n = BigInteger.valueOf(i);
			for (int j = 2; j <= i; j++) {
				n = n.multiply(BigInteger.valueOf(i)).mod(t);
			}
			sum = (sum.add(n)).mod(t);
		}
		return sum.toString();
	}


	/****************************************************************************
	 Problem 49:
	 """
	 The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
	 increases by 3330, is unusual in two ways: (i) each of the three terms are
	 prime, and, (ii) each of the 4-digit numbers are permutations of one another.

	 There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
	 exhibiting this property, but there is one other 4-digit increasing sequence.

	 What 12-digit number do you form by concatenating the three terms
	 in this sequence?
	 """
	 ****************************************************************************/
	// 0.0625s
	@SuppressWarnings("unused")
	public static String euler49() {
		int diff = 3330;
		String res = "";
		for (int n = 1001; n <= 9999; n += 2) {
			String c = e49_check_perms(n, diff);
			if (n != 1487 &&
					EulerUtils.is_prime_simple(n) &&
					c.length() != 0
					) {
				res = c;
			}
		}
		return res;
	}

	// Represents numbers as string to simplify the permutations etc
	public static String e49_check_perms(int n, int diff) {
		List<String> all_perms = EulerUtils.all_permutations(Integer.toString(n));
		String ll = "";
		if (all_perms.size() > 0) {
			String p1 = e49_get_element(n, all_perms, diff);
			String p2 = "";
			if (p1.length() > 0) {
				p2 = e49_get_element(Integer.parseInt(p1), all_perms, diff);
			}
			if (p2.length() > 0) {
				ll = Integer.toString(n) + p1 + p2;
			}
		}
		return ll;
	}

	// Represents the number as strings...
	public static String e49_get_element(int n, List<String> ll, int diff) {
		String res = "";
		for (String p : ll) {
			int pp = Integer.parseInt(p);
			if (pp > n && pp - n == diff) {
				res = p;
			}
		}
		return res;
	}


	/****************************************************************************
	 Problem 50:
	 """
	 The prime 41, can be written as the sum of six consecutive primes:
	 41 = 2 + 3 + 5 + 7 + 11 + 13

	 This is the longest sum of consecutive primes that adds to a prime
	 below one-hundred.

	 The longest sum of consecutive primes below one-thousand that adds to a prime,
	 contains 21 terms, and is equal to 953.

	 Which prime, below one-million, can be written as the sum of the most
	 consecutive primes?
	 """
	 ****************************************************************************/
	// 0.0293s
	@SuppressWarnings("unused")
	public static String euler50() {
		int n = 10_000;
		List<Integer> primes = new ArrayList<>();
		primes.add(2);
		for (int i = 3; i <= n; i += 2) {
			if (EulerUtils.is_prime_simple(i)) {
				primes.add(i);
			}
		}
		int found = 0;
		for (int len = 550; len >= 21; len--) {
			if (found > 0) {
				break;
			}
			for (int offset = 1; offset <= 549; offset++) {
				if (found > 0) {
					break;
				}

				int sum = 0;
				for (int j = offset + 1; j <= offset + len; j++) {
					sum += primes.get(j);
				}
				if (sum < 1_000_000 && EulerUtils.is_prime_simple(sum)) {
					found = sum;
				}
			}
		}
		return Integer.toString(found);
	}

	/*

	 */

}
