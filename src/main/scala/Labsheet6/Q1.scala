package Labsheet6

import scala.io.StdIn.{readChar, readInt, readLine}

object Q1 {
  def shiftChar(x: Char, key: Int): Char = {
    if (x.isLetter) {
      if (x.isUpper)
        ((x - 'A' + key) % 26 + 'A').toChar;
      else
        ((x - 'a' + key) % 26 + 'a').toChar;
    } else
      x;
  }

  def encrypt(plaintext: String, key: Int): String = {
    if (plaintext.isEmpty)
      ""
    else
      shiftChar(plaintext.head, key) + encrypt(plaintext.tail, key);
  }

  def decrypt(ciphertext: String, key: Int): String = {
    encrypt(ciphertext, 26 - key)
  }

  def main(args: Array[String]): Unit = {
    print("For encryption - Press 'e' \nFor decryption - Press 'd': ");
    var choice = readChar();

    if(choice == 'e'){
      print("Enter your plaintext: ");
      var input = readLine();

      print("Enter the key: ");
      var shift = readInt();

      var result = encrypt(input, shift);

      println("The encrypted text: " + result);
    }
    else if(choice == 'd'){
      print("Enter your ciphertext: ");
      var input = readLine();

      print("Enter the key: ");
      var shift = readInt();

      var result = decrypt(input, shift);

      println("The encrypted text: " + result);
    }
    else
      print("Please enter a valid input");
  }
}
