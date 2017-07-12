import java.lang.reflect.Method;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.IntStream;

import static java.lang.String.format;

/**
 * Created by hakank on 2017-04-14.
 */
class EulerUtils {

	public static Map<String, String> answers = new HashMap<>();

	public static void init_answers() {
		answers.put("1","233168");
		answers.put("2","4613732");
		answers.put("3","6857");
		answers.put("4","906609");
		answers.put("5","232792560");
		answers.put("6","25164150");
		answers.put("7","104743");
		answers.put("8","40824");
		answers.put("9","31875000");
		answers.put("10","142913828922");
		answers.put("11","70600674");
		answers.put("12","76576500");
		answers.put("13","5537376230");
		answers.put("14","837799");
		answers.put("15","137846528820");
		answers.put("16","1366");
		answers.put("17","21124");
		answers.put("18","1074");
		answers.put("19","171");
		answers.put("20","648");
		answers.put("21","31626");
		answers.put("22","871198282");
		answers.put("23","4179871");
		answers.put("24","2783915460");
		answers.put("25","4782");
		answers.put("26","983");
		answers.put("27","-59231");
		answers.put("28","669171001");
		answers.put("29","9183");
		answers.put("30","443839");
		answers.put("31","73682");
		answers.put("32","45228");
		answers.put("33","100");
		answers.put("34","40730");
		answers.put("35","55");
		answers.put("36","872187");
		answers.put("37","748317");
		answers.put("38","932718654");
		answers.put("39","840");
		answers.put("40","210");
		answers.put("41","7652413");
		answers.put("42","162");
		answers.put("43","16695334890");
		answers.put("44","5482660");
		answers.put("45","1533776805");
		answers.put("46","5777");
		answers.put("47","134043");
		answers.put("48","9110846700");
		answers.put("49","296962999629");
		answers.put("50","997651");
	}

	public static int num_more_1s = 0; // more than 1s
	public static int num_more_01s = 0; // more than 0.1s
	public static List<String> num_more_1s_l = new ArrayList<>();
	public static List<String> num_more_01s_l = new ArrayList<>();
	public static int num_errors = 0;
	public static List<String> error_problems = new ArrayList<>();

	public static double total_time = 0;
	// Testing an Euler problem
	public static void testEuler(String f) {
		String pattern = "^euler(\\d+)([^\\d]?)$";
		Pattern r = Pattern.compile(pattern);
		Matcher m = r.matcher(f);
		String correct_answer = "";
		if (m.find()) {
			String problem_no = m.group(1);
			String solution = answers.get(problem_no);
			System.out.println("Problem #"+ problem_no + ". Solution should be " + solution);
			correct_answer = solution;
		}
		System.out.printf("\tTesting %10s: ", f);
		// System.gc();
		Timer.tic();
		try {
			Method method = Main.class.getMethod(f);
			String result = (String)method.invoke(null);
			double time = Timer.toc();
			total_time += time;
			System.out.println("Result: " + result);
			String time_formatted = String.format("%10.3f", time);
			String message = "";
			String correct_message = "OK";
			if (result.compareTo(correct_answer) != 0) {
				// System.out.println("WRONG ANSWER!");
				correct_message = "WRONG ANSWER: should be " + correct_answer;
				num_errors++;
				error_problems.add(f + " Result was " + result + ", should be " + correct_answer);
			}

			if (time >= 1.0) {
				// Warning for slow programs
				message = "(!!)";
				num_more_1s++;
				num_more_1s_l.add(f + " " + time_formatted);
			} else if (time >= 0.1) {
				message = "(!)";
				num_more_01s++;
				num_more_01s_l.add(f + " " + time_formatted);
			}
			System.out.printf("%9.3f seconds %s status: %s", time, message, correct_message);

		} catch (Exception e) {
			e.printStackTrace();
		}
		System.out.println();
		// return time;
	}



	public static boolean isPalindrome(String s) {
		int n = s.length();
		for (int i = 0; i < (n / 2); ++i) {
			if (s.charAt(i) != s.charAt(n - i - 1)) {
				return false;
			}
		}
		return true;
	}

	// http://stackoverflow.com/questions/4407839/how-can-i-find-the-square-root-of-a-java-biginteger
	public static BigInteger sqrt(BigInteger n) {
		BigInteger a = BigInteger.ONE;
		BigInteger b = new BigInteger(n.shiftRight(5).add(new BigInteger("8")).toString());
		while (b.compareTo(a) >= 0) {
			BigInteger mid = new BigInteger(a.add(b).shiftRight(1).toString());
			if (mid.multiply(mid).compareTo(n) > 0)
				b = mid.subtract(BigInteger.ONE);
			else
				a = mid.add(BigInteger.ONE);
		}
		return a.subtract(BigInteger.ONE);
	}

	// http://stackoverflow.com/questions/7875380/recursive-fibonacci-memoization
	public static int fib(int n, Map<Integer, Integer> map) {

		if (n == 0 || n == 1) {
			return n;
		}

		if (map.containsKey(n)) {
			return map.get(n);
		}

		int t = fib(n - 1, map) + fib(n - 2, map);
		map.put(n, t);
		return t;

	}

	// Refactored to support BigInteger
	public static BigInteger fibB(BigInteger n, Map<BigInteger, BigInteger> map) {

		if (n.equals(BigInteger.ZERO) || n.equals(BigInteger.ONE)) {
			return BigInteger.ONE;
		}

		if (map.containsKey(n)) {
			return map.get(n);
		}

		// yuck!
		BigInteger t = fibB(n.add(BigInteger.valueOf(-1)), map).add(fibB(n.add(BigInteger.valueOf(-2)), map));
		map.put(n, t);
		return t;

	}

	// Fibonacci using arrays
	public static int fibA(int n) {
		if (n == 0 || n == 1) { return 1; }
		int[] f = new int[n];
		f[0] = 1;
		f[1] = 1;
		for(int i = 2; i < n; i++) {
			f[i] = f[i-1] + f[i-2];
		}
		return f[n-1];
	}

	// For n > 92 go BigInteger
	public static BigInteger fibABig(int n) {
		if (n == 0 || n == 1) {
			return BigInteger.ONE;
		}
		BigInteger[] f = new BigInteger[n];
		f[0] = BigInteger.ONE;
		f[1] = BigInteger.ONE;
		for(int i = 2; i < n; i++) {
			f[i] = f[i-1].add(f[i-2]);
		}
		return f[n-1];
	}

	// lcm(a,b)
	public static int lcm(int a, int b) {
		// lcm = (a*b)//gcd(a,b)
		return (a*b)/gcd(a,b);

	}

	public static int gcd(int a, int b) {
		return a == 0 ? b : gcd(b % a, a);
	}

	// lcm(a,b) BigInteger
	public static BigInteger lcmB(BigInteger a, BigInteger b) {
		// lcm = (a*b)//gcd(a,b)
		return a.multiply(b).divide(a.gcd(b));
	}


	// For euler11
	public static int[] get_row(int[][] x, int row ) {
		return x[row];
	}

	// For euler11
	public static int[] get_col(int[][] x, int col) {
		int len = x.length;
		int[] t = new int[len];
		for(int j = 0; j < len; j++) {
			t[j] = x[j][col];
		}
		return t;
	}

	// for euler11
	public static int max_running_prod(int[] x, int len) {
		int max = 0;
		// int p = 1;
		for(int i = 0; i < (x.length - len - 1); i++) {
			// System.out.println("i: " + i);
			int p = 1;
			for (int j = i; j < i + len; j++) {
				// System.out.println("\tj: " + j + " : " + x[j]);
				p *= x[j];
			}
			// System.out.println("\t\tp: " + p);
			if (p > max) {
				max = p;
			}

		}
		return max;

	}


	// For euler11
	// get all the 1st diagonals, j is the j'th diagonal
	public static int get_diags1(int[][] x, int s, int j) {
		int len = x[0].length;
		int max = 0;
		for(int i = 0; i < len-s+1; i++) {
			int p = 1;
			for(int a = 0; a < s; a++) {
				p *= x[a+i][a+j];
				if (p > max) {
					max = p;
				}
			}
		}
		return max;
	}

	// For euler11
	// get all the 2nd diagonals, j is the j'th diagonal
	public static int get_diags2(int[][] x, int s, int j) {
		int len = x[0].length;
		int max = 0;
		for(int i = s-1; i < len; i++) {
			int p = 1;
			for(int a = 0; a < s; a++) {
				p *= x[i-a][j+a];
				if (p > max) {
					max = p;
				}
			}
		}
		return max;
	}

	// number of divisors of a number (int)
	// number of divisors = multiply the (exponents + 1)
	public static int num_divisors(int n) {
		Map<Integer,Integer> h = factors(n);
		int num_divs = 1;
		for(Map.Entry<Integer,Integer> s: h.entrySet()) {
			num_divs *= s.getValue() + 1;
		}
		return num_divs;
	}

	// Factors of a number (int)
	public static Map<Integer,Integer> factors(int n) {
		int m = n;
		Map<Integer, Integer> map = new HashMap<>();
		int t = 2;
		while (m > 1 && t < Math.ceil(Math.sqrt((double)m))) {
			while (m % t == 0) {
				map.put(t, map.getOrDefault(t,0)+1);
				m /= t;
			}
			if (t == 2) {
				t++; // 2 -> 3
			} else {
				t += 2; // now check only odd numbers
			}
		}
		if (m > 1) {
			map.put(m, map.getOrDefault(t,0)+1);
		}
		return map;

	} // end factors

	// _very_ slow
	public static long num_divisors_slow(int n) {
		return
				IntStream.rangeClosed(1,n/2)
						.parallel()
						.filter(i -> n % i == 0)
						.count()+1;
	}


	public static final String[] euler13_nums = {
			"37107287533902102798797998220837590246510135740250",
			"46376937677490009712648124896970078050417018260538",
			"74324986199524741059474233309513058123726617309629",
			"91942213363574161572522430563301811072406154908250",
			"23067588207539346171171980310421047513778063246676",
			"89261670696623633820136378418383684178734361726757",
			"28112879812849979408065481931592621691275889832738",
			"44274228917432520321923589422876796487670272189318",
			"47451445736001306439091167216856844588711603153276",
			"70386486105843025439939619828917593665686757934951",
			"62176457141856560629502157223196586755079324193331",
			"64906352462741904929101432445813822663347944758178",
			"92575867718337217661963751590579239728245598838407",
			"58203565325359399008402633568948830189458628227828",
			"80181199384826282014278194139940567587151170094390",
			"35398664372827112653829987240784473053190104293586",
			"86515506006295864861532075273371959191420517255829",
			"71693888707715466499115593487603532921714970056938",
			"54370070576826684624621495650076471787294438377604",
			"53282654108756828443191190634694037855217779295145",
			"36123272525000296071075082563815656710885258350721",
			"45876576172410976447339110607218265236877223636045",
			"17423706905851860660448207621209813287860733969412",
			"81142660418086830619328460811191061556940512689692",
			"51934325451728388641918047049293215058642563049483",
			"62467221648435076201727918039944693004732956340691",
			"15732444386908125794514089057706229429197107928209",
			"55037687525678773091862540744969844508330393682126",
			"18336384825330154686196124348767681297534375946515",
			"80386287592878490201521685554828717201219257766954",
			"78182833757993103614740356856449095527097864797581",
			"16726320100436897842553539920931837441497806860984",
			"48403098129077791799088218795327364475675590848030",
			"87086987551392711854517078544161852424320693150332",
			"59959406895756536782107074926966537676326235447210",
			"69793950679652694742597709739166693763042633987085",
			"41052684708299085211399427365734116182760315001271",
			"65378607361501080857009149939512557028198746004375",
			"35829035317434717326932123578154982629742552737307",
			"94953759765105305946966067683156574377167401875275",
			"88902802571733229619176668713819931811048770190271",
			"25267680276078003013678680992525463401061632866526",
			"36270218540497705585629946580636237993140746255962",
			"24074486908231174977792365466257246923322810917141",
			"91430288197103288597806669760892938638285025333403",
			"34413065578016127815921815005561868836468420090470",
			"23053081172816430487623791969842487255036638784583",
			"11487696932154902810424020138335124462181441773470",
			"63783299490636259666498587618221225225512486764533",
			"67720186971698544312419572409913959008952310058822",
			"95548255300263520781532296796249481641953868218774",
			"76085327132285723110424803456124867697064507995236",
			"37774242535411291684276865538926205024910326572967",
			"23701913275725675285653248258265463092207058596522",
			"29798860272258331913126375147341994889534765745501",
			"18495701454879288984856827726077713721403798879715",
			"38298203783031473527721580348144513491373226651381",
			"34829543829199918180278916522431027392251122869539",
			"40957953066405232632538044100059654939159879593635",
			"29746152185502371307642255121183693803580388584903",
			"41698116222072977186158236678424689157993532961922",
			"62467957194401269043877107275048102390895523597457",
			"23189706772547915061505504953922979530901129967519",
			"86188088225875314529584099251203829009407770775672",
			"11306739708304724483816533873502340845647058077308",
			"82959174767140363198008187129011875491310547126581",
			"97623331044818386269515456334926366572897563400500",
			"42846280183517070527831839425882145521227251250327",
			"55121603546981200581762165212827652751691296897789",
			"32238195734329339946437501907836945765883352399886",
			"75506164965184775180738168837861091527357929701337",
			"62177842752192623401942399639168044983993173312731",
			"32924185707147349566916674687634660915035914677504",
			"99518671430235219628894890102423325116913619626622",
			"73267460800591547471830798392868535206946944540724",
			"76841822524674417161514036427982273348055556214818",
			"97142617910342598647204516893989422179826088076852",
			"87783646182799346313767754307809363333018982642090",
			"10848802521674670883215120185883543223812876952786",
			"71329612474782464538636993009049310363619763878039",
			"62184073572399794223406235393808339651327408011116",
			"66627891981488087797941876876144230030984490851411",
			"60661826293682836764744779239180335110989069790714",
			"85786944089552990653640447425576083659976645795096",
			"66024396409905389607120198219976047599490197230297",
			"64913982680032973156037120041377903785566085089252",
			"16730939319872750275468906903707539413042652315011",
			"94809377245048795150954100921645863754710598436791",
			"78639167021187492431995700641917969777599028300699",
			"15368713711936614952811305876380278410754449733078",
			"40789923115535562561142322423255033685442488917353",
			"44889911501440648020369068063960672322193204149535",
			"41503128880339536053299340368006977710650566631954",
			"81234880673210146739058568557934581403627822703280",
			"82616570773948327592232845941706525094512325230608",
			"22918802058777319719839450180888072429661980811197",
			"77158542502016545090413245809786882778948721859617",
			"72107838435069186155435662884062257473692284509516",
			"20849603980134001723930671666823555245252804609722",
			"53503534226472524250874054075591789781264330331690"
	};


	// for int (don't work in euler14)
	public static int hailstone(int n) {
		if (n % 2 == 0) {
			return n / 2;
		} else {
			return (3*n)+1;
		}
	}

	// hailstone(n) BigInteger
	public static BigInteger hailstoneB(BigInteger n) {
		BigInteger one = BigInteger.ONE;
		BigInteger two = BigInteger.valueOf(2);
		BigInteger three = BigInteger.valueOf(3);
		if (n.mod(two).compareTo(BigInteger.ZERO) == 0) {
			// n / 2
			return n.divide(two);
		} else {
			// (n*3) + 1 (note the parenthesis!)
			return (n.multiply(three).add(one));
		}
	}


	public static String english(int N) {

		int[] Divs  =  {1000_000_000, 1_000_000,  1000,       100};
		String[] Divnames  =  {"billion", "million", "thousand", "hundred"};
		String[] Prefixes  =  {"","0", "twen", "thir", "for", "fif", "six", "seven", "eigh", "nine"};
		String[]_Ordinals  = {"","first", "second", "third", "fourth", "fifth", "sixth", "seventh",
				"eighth", "ninth", "tenth", "eleventh", "twelfth", "thirteenth",
				"fourteenth","fifteenth", "sixteenth", "seventeenth",
				"eighteenth", "nineteenth"};
		String[] Cardinals =  {"","one", "two", "three", "four", "five", "six", "seven",
				"eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
				"fifteen", "sixteen", "seventeen", "eighteen", "nineteen"};

		String Sstr = "";
		int Printed = 0;
		if (N < 0) {
			Sstr = "minus" + Sstr;
			N = -N;
		}
		int D;
		for(int I = 0; I < Divs.length; I++) {
			D = N / Divs[I];
			N = N % Divs[I];
			if (D != 0) {
				Sstr += english(D) + Divnames[I];
				Printed =1;
			}
		}

		if (N > 0 && Printed == 1) {
			Sstr = Sstr + "and";
		}
		if (N == 0) {
			// dummy
		} else if (N > 19) {
			D = N / 10;
			N = N % 10;
			Sstr = Sstr + Prefixes[D] + "ty"+ english(N);
		} else {
			Sstr =Sstr + Cardinals[N];
		}

		return Sstr;

	}

	// Assumption: all characters are digits in base 10
	public static int sum_string(String s) {
		int sum = 0;
		for(char c: s.toCharArray()) {
			sum += Character.getNumericValue(c);
		}
		return sum;
	}

	// Assumption: all characters are digits in base 10
	public static BigInteger sum_stringB(String s) {
		BigInteger sum = BigInteger.ZERO;
		for(char c: s.toCharArray()) {
			sum = sum.add(BigInteger.valueOf(Character.getNumericValue(c)));
		}
		return sum;
	}


	// int factorial
	public static int factorial(int n) {
		int t = 1;
		for(int i = 2; i <= n; i++ ) {
			t *= i;
		}
		return t;
	}


	// BigInteger factorial
	public static BigInteger factorialB(int n) {
		BigInteger t = BigInteger.ONE;
		for(int i = 2; i <= n; i++ ) {
			t = t.multiply(BigInteger.valueOf(i));
		}
		return t;
	}


	// Sum of divisors (int n)
	public static int sum_divisors(int n) {
		int d = (int)Math.floor(Math.sqrt(n));
		int sum = 1;
		for(int i = 2; i <= d; i++) {
			if (n % i == 0) {
				sum += i;
				if (i != n % i) {
					sum += n / i;
				}
			}
		}
		return sum;
	}

	// for euler22
	public static int alpha_pos(char c) {
		return Character.getNumericValue(c)-9;
	}

	public static int sum_alpha_pos(String s) {
		int sum = 0;
		for(char c: s.toCharArray()) {
			sum += EulerUtils.alpha_pos(c);
		}
		return sum;
	}

	// for euler24
	// Based on
	// answer at http://stackoverflow.com/questions/4240080/generating-all-permutations-of-a-given-string
	public static List<String> permutation(String str) {
		List<String> strings = new ArrayList<>();
		permutation("", str,strings);
		return strings;
	}

	private static List<String> permutation(String prefix, String str, List<String> strings) {
		int n = str.length();
		if (n == 0) strings.add(prefix);
		else {
			for (int i = 0; i < n; i++)
				permutation(prefix + str.charAt(i), str.substring(0, i) + str.substring(i+1, n),strings);
		}
		return strings;
	}

	public static int[] next_permutation(int[] p) {
		int i = p.length - 1;
		while (i > 0 && p[i - 1] >= p[i])
			i--;
		if (i <= 0) {
			return null;
		}

		int j = p.length - 1;
		while (p[j] <= p[i - 1]) {
			j--;
		}

		// swap
		int t = p[i - 1];
		p[i - 1] = p[j];
		p[j] = t;

		j = p.length - 1;
		while (i < j) {
			// swap
			t = p[i];
			p[i] = p[j];
			p[j] = t;
			i++;
			j--;
		}

		return p;
	}


	// permutation of List<Integer>
	public static List next_permutation(List<Integer> p) {
		int len = p.size();
		int i = len - 1;
		while (i > 0 && p.get(i - 1) >= p.get(i))
			i--;
		if (i <= 0) {
			return null;
		}

		int j = len - 1;
		while (p.get(j) <= p.get(i - 1)) {
			j--;
		}

		// swap
		int t = p.get(i - 1);
		p.add(i - 1,p.get(j));
		p.add(j,t);

		j = len - 1;
		while (i < j) {
			// swap
			t = p.get(i);
			p.add(i,p.get(j));
			p.add(j,t);
			i++;
			j--;
		}

		return p;
	}


	public static String next_permutation(String p) {
		int len = p.length();
		int i = len - 1;
		while (i > 0 && p.charAt(i - 1) >= p.charAt(i)) {
			i--;
		}
		if (i <= 0) {
			return null;
		}

		int j = len - 1;
		while (p.charAt(j) <= p.charAt(i - 1)) {
			j--;
		}

		// swap
		char t = p.charAt(i - 1);
		p.replace(p.charAt(i - 1),p.charAt(j));
		p.replace(p.charAt(j),t);

		j = len - 1;
		while (i < j) {
			// swap
			t = p.charAt(i);
			p.replace(p.charAt(i),p.charAt(j));
			p.replace(p.charAt(j),t);
			i++;
			j--;
		}

		return p;
	}

	// Inspired by an answer from
	// http://stackoverflow.com/questions/4240080/generating-all-permutations-of-a-given-string
	public static List<String> all_permutations(String str) {
		List<String> perms = new ArrayList<>();
		permutation_tmp("", str, perms);
		return perms;
	}

	private static void permutation_tmp(String prefix, String str, List<String> perms) {
		int n = str.length();
		if (n == 0) {
			perms.add(prefix);
		}
		else {
			for (int i = 0; i < n; i++)
				permutation_tmp(prefix + str.charAt(i), str.substring(0, i) + str.substring(i+1, n), perms);
		}

	}

	/* // DONT' WORK (as I expect it)
	public static List<List<Integer>> all_permutations(List<Integer> s) {
		List<List<Integer>> perms = new ArrayList<>();
		List<Integer> empty= new ArrayList<>();
		permutation_tmp(empty, s, perms);
		return perms;
	}

	private static void permutation_tmp(List<Integer> prefix, List<Integer>s, List<List<Integer>> perms) {
		int n = s.size();
		if (n == 0) {
			perms.add(prefix);
		}
		else {
			for (int i = 0; i < n; i++)
				permutation_tmp(prefix.add(s.get(i)), s.subList(0, i).add(s.subList(i+1, n)), perms);
		}
	}
*/

	public static int fib_length(int n) {
		return fibABig(n).toString().length();
	}


	// For euler26

	// Simple prime check
	public static boolean is_prime_simple(int n) {
		if (n == 2 || n == 3 || n == 5 || n == 7) {
			return true;
		}
		if (n == 1 || n % 2 == 0) {
			return false;
		}
		for (int i = 3; i < Math.ceil(Math.sqrt(n))+1; i+=2) {
			if (n % i == 0) {
				return false;
			}
		}
		return true;
	}

	// Get the length of a repetition of digits of 1/n
	public static int get_rep_len(int n) {
		int[] found_remainders = new int[n+1];
		for(int i = 0; i <= n; i++) {
			found_remainders[i] = 0;
		}
		int value = 1;
		int position = 0;
		while (found_remainders[value] == 0 &&value != 0) {
			found_remainders[value] = position;
			value = (value*10) % n;
			position++;
		}

		return position-found_remainders[value];
	}


	// for euler30
	// sum all the digits in a string powered by pow
	// i.e.    12345 -> 1^pow + 2^pow + 3^pow + 4^pow + 5^pow
	public static int sum_string_digits_pow(int n, int pow) {
		int sum = 0;
		for(char c : Integer.toString(n).toCharArray()) {
			sum += Math.ceil(Math.pow(Character.getNumericValue(c),pow));
		}
		return sum;
	}

	// returns int[] of 0..n
	// Perhaps is should be 0..n-1 or 1..n!
	public static int[] range_array(int n) {
		int[] r = new int[n];
		for(int i = 0; i < n; i++) {
			r[i] = i;
		}
		return r;
	}

	// returns int[] of from..to
	public static int[] range_array(int from, int to) {
		int limit = to-from+1;
		int[] r = new int[limit];
		for(int i = from; i <= limit; i++) {
			r[i-from] = i;
		}
		return r;
	}

	// Note: returns Integer[] (not int[]) of from..step..to
	public static Integer[] range_array2(int from, int to, int step) {
		List<Integer> r = new ArrayList<>();
		int limit = to-from+1;
		for(int i = from; i <= to; i = i + step) {
			r.add(i);
		}
		return r.toArray(new Integer[r.size()]);
	}

	public static int[] range_array(int from, int to, int step) {
		int limit = (to-from+step) /step;
		int[] r = new int[limit];
		int c = 0;
		for(int i=from; i<=to; i=i+step) {
			r[c++]=i;
		}
		return r;

	}


	public static int sum_array(int[] a) {
		int sum = 0;
		for(int i: a) {
			sum += i;
		}
		return sum;
	}

	// For euler42
	public static int triangle_number(int n) {
		return (n*(n+1)) / 2;
	}


	public static int e42_get_score(String name) {
		int total = 0;
		for(int i = 0; i < name.length(); i++) {
			total += ((int)name.charAt(i))-64;
		}
		return total;
	}



}


/* From http://stackoverflow.com/questions/180158/how-do-i-time-a-methods-execution-in-java */
class Timer {
	private static long start_time;

	public static double tic() {
		return start_time = System.nanoTime();
	}

	public static double toc() {
		return (System.nanoTime() - start_time) / 1000000000.0;
	}

}

