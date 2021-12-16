import java.util.Scanner;
import java.util.List;

class CollectVersions implements BitStream.Visitor<Long> {
	public Long onLiteral(int version, long value) {
		return (long)version;
	}

	public Long onOperator(int version, int type, List<Long> operands)
	{
		long total = version;
		for (long o: operands) {
			total += o;
		}
		return total;
	}
}

public class Part1
{
	public static void main(String[] args) {
		Scanner input = new Scanner(System.in);
		while (input.hasNextLine()){
			BitStream stream = new BitStream(input.nextLine());
			System.out.println(stream.readPacket(new CollectVersions()));
		}
	}
}
