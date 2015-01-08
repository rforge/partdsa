package dsa;

public class Hex {
  public static byte[] decodeHex(String s) throws IllegalArgumentException {
    char[] data = s.toCharArray();
    int len = data.length;

    if ((len & 0x01) != 0) {
      throw new IllegalArgumentException("odd number of characters");
    }

    byte[] out = new byte[len >> 1];

    for (int i = 0, j = 0; j < len; i++) {
      int f = toDigit(data[j], j) << 4;
      j++;
      f = f | toDigit(data[j], j);
      j++;
      out[i] = (byte) (f & 0xFF);
    }

    return out;
  }

  private static int toDigit(char ch, int index) throws IllegalArgumentException {
    int digit = Character.digit(ch, 16);
    if (digit == -1) {
      throw new IllegalArgumentException("illegal hexadecimal character " + ch +
                                         " at index " + index);
    }
    return digit;
  }
}
