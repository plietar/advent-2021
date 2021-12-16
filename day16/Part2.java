import java.util.Scanner;
import java.util.List;
import java.util.Collections;

class Evaluate implements BitStream.Visitor<Long> {
	public Long onLiteral(int version, long value) {
		return value;
	}

	private static Long sum(List<Long> operands) {
		long result = 0;
		for (long o : operands) {
			result += o;
		}
		return result;
	}

	private static Long product(List<Long> operands) {
		long result = 1;
		for (long o : operands) {
			result *= o;
		}
		return result;
	}

	public Long onOperator(int version, int type, List<Long> operands)
	{
		switch (type) {
			case 0: return sum(operands);
			case 1: return product(operands);
			case 2: return Collections.min(operands);
			case 3: return Collections.max(operands);
			case 5: return (operands.get(0) > operands.get(1)) ? (long)1 : (long)0;
			case 6: return (operands.get(0) < operands.get(1)) ? (long)1 : (long)0;
			case 7: return (operands.get(0) == operands.get(1)) ? (long)1 : (long)0;
			default: throw new IllegalArgumentException();
		}
	}
}


public class Part2
{
	public static void main(String[] args) {
		Scanner input = new Scanner(System.in);
		while (input.hasNextLine()){
			BitStream stream = new BitStream(input.nextLine());
			System.out.println(stream.readPacket(new Evaluate()));
		}
	}
}
