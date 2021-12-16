import java.util.List;
import java.util.ArrayList;

public class BitStream
{
	public interface Visitor<T> {
		T onLiteral(int version, long value);
		T onOperator(int version, int type, List<T> operands);
	}

	static byte fromHex(char c) {
		if (c >= '0' && c <= '9') {
			return (byte)(c - '0');
		} else if (c >= 'A' && c <= 'F') {
			return (byte)(10 + c - 'A');
		} else if (c >= 'a' && c <= 'f') {
			return (byte)(10 + c - 'a');
		} else {
			throw new IllegalArgumentException();
		}
	}

	private static byte fromHex(char hi, char lo) {
		return (byte)((fromHex(hi) << 4) | fromHex(lo));
	}

	public BitStream(String input) {
		data = new byte[input.length() * 4];
		for (int i = 0; i < input.length(); i += 1) {
			byte b = fromHex(input.charAt(i));
			data[4*i+0] = ((b & 0x8) != 0) ? (byte)1 : (byte)0;
			data[4*i+1] = ((b & 0x4) != 0) ? (byte)1 : (byte)0;
			data[4*i+2] = ((b & 0x2) != 0) ? (byte)1 : (byte)0;
			data[4*i+3] = ((b & 0x1) != 0) ? (byte)1 : (byte)0;
		}
	}

	public long read(int n) {
		long result = 0;
		for (int i = 0; i < n; i++) {
			result |= ((long)data[offset + i] << (n - i -1));
		}
		offset += n;
		return result;
	}

	public long readLiteral() {
		long result = 0;
		while (true) {
			long group = read(5);
			result = (result << 4) | (group & 0xf);
			if ((group & 0x10) == 0) {
				return result;
			}
		}
	}

	public <T> T readPacket(Visitor<T> handler) {
		int version = (int)read(3);
		int type = (int)read(3);
		if (type != 4) {
			ArrayList<T> operands = new ArrayList<T>();
			long mode = read(1);
			if (mode == 0) {
				long length = read(15);
				for (int start = offset; offset < start + length;) {
					operands.add(readPacket(handler));
				}
			} else {
				long count = read(11);
				for (int i = 0; i < count; i++) {
					operands.add(readPacket(handler));
				}
			}
			return handler.onOperator(version, type, operands);
		} else {
			long value = readLiteral();
			return handler.onLiteral(version, value);
		}
	}

	private byte[] data;
	private int offset = 0;
}
