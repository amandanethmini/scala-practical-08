object cypher {
  def encrypt(plaintext: String, shift: Int): String = {
    plaintext.map { char =>
      if (char.isLetter) {
        val shiftBase = if (char.isUpper) 'A' else 'a'
        ((char - shiftBase + shift) % 26 + shiftBase).toChar
      } else {
        char
      }
    }
  }

  def decrypt(ciphertext: String, shift: Int): String = {
    ciphertext.map { char =>
      if (char.isLetter) {
        val shiftBase = if (char.isUpper) 'A' else 'a'
        ((char - shiftBase - shift + 26) % 26 + shiftBase).toChar
      } else {
        char
      }
    }
  }

  def cipher(
      data: String,
      shift: Int,
      operation: (String, Int) => String
  ): String = {
    operation(data, shift)
  }

  def main(args: Array[String]): Unit = {
    println("Enter text:")
    val text = scala.io.StdIn.readLine()

    println("Enter shift value:")
    val shift = scala.io.StdIn.readInt()

    println("Choose an option: (1) Encrypt, (2) Decrypt")
    val choice = scala.io.StdIn.readInt()

    val result = choice match {
      case 1 => cipher(text, shift, encrypt)
      case 2 => cipher(text, shift, decrypt)
      case _ => "Invalid option selected."
    }

    println(s"Result: $result")
  }
}
