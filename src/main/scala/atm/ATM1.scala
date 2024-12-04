import scala.io.StdIn

object Exo1 extends App {
  var choice = 0
  val correctPin = "INTRO1234"
  var balance = 1200.0
  var isFirstOperation = true

  while (choice != 4) {
    println("Choose your operation:")
    println("1) Deposit")
    println("2) Withdrawal")
    println("3) Account consultation")
    println("4) Finish")
    print("Your choice > ")
    choice = StdIn.readInt()

    if (isFirstOperation && choice != 4) {
      isFirstOperation = false
      var remainingAttempts = 3
      print("Enter your pin code > ")
      var enteredPin = StdIn.readLine()

      while (enteredPin != correctPin) {
        remainingAttempts -= 1
        if (remainingAttempts > 0) {
          println(s"Invalid PIN code, you have $remainingAttempts attempts remaining.")
          enteredPin = StdIn.readLine()
        } else {
          println("For your protection, banking operations will be suspended, please retrieve your card.")
          System.exit(-1)
        }
      }
      println("PIN code OK.")
    }

    if (choice == 1) {
      var currency = ""
      var depositAmount = 0.0
      while (currency != "1" && currency != "2") {
        print("Indicate the currency of the deposit: 1) CHF; 2) EUR > ")
        currency = StdIn.readLine()
      }

      print("Indicate the amount of the deposit > ")
      depositAmount = StdIn.readInt()

      while (depositAmount % 10 != 0 || depositAmount < 10) {
        println("The amount must be a multiple of 10.")
        depositAmount = StdIn.readInt()
      }
      if (currency == "2") depositAmount *= 0.95
      balance += depositAmount
      println(s"Your deposit has been processed. The new amount available on your account is: $balance\n\n")
    } else if (choice == 2) {
      val withdrawalLimit = balance * 0.10
      var currency = ""
      var currencyString = ""

      while (currency != "1" && currency != "2") {
        print("Indicate the currency: 1) CHF; 2) EUR > ")
        currency = StdIn.readLine()
      }
      if (currency == "1") currencyString = "CHF" else currencyString = "EUR"
      print("Indicate the amount of the withdrawal > ")
      var withdrawalAmount = StdIn.readInt()

      while (withdrawalAmount % 10 != 0 || withdrawalAmount > withdrawalLimit) {
        if (withdrawalAmount % 10 != 0) {
          println("The amount must be a multiple of 10.")
        } else if (withdrawalAmount > withdrawalLimit) {
          println(s"Your authorized withdrawal limit is: $withdrawalLimit")
        }
        withdrawalAmount = StdIn.readInt()
      }

      var denominationsType = ""

      if (withdrawalAmount < 200 || currency == "2") { // euro ou retrait < 200
        denominationsType = "2";
      } else {
        while (denominationsType != "1" && denominationsType != "2") {
          print("In 1) large denominations, 2) small denominations > ")
          denominationsType = StdIn.readLine()
        }
      }
      var remainingAmount = withdrawalAmount
      var maxCount = 0
      var withdrawalString = ""
      var count = 0
      var denominationProposed = 0

      if (denominationsType == "2" || currencyString == "EUR" || remainingAmount < 200) {
        denominationProposed = 100
      } else { // large denominations et retrait >= 200
        denominationProposed = 500
      } 

      while (remainingAmount > 0) {
        maxCount = (remainingAmount / denominationProposed).toInt
        while (maxCount <= 0) {
          if (denominationProposed == 500 || denominationProposed == 50)
            denominationProposed = (denominationProposed / 2.5).toInt
          else denominationProposed = denominationProposed / 2
          maxCount = (remainingAmount / denominationProposed).toInt
        }
        if (denominationProposed == 10) {
          withdrawalString += s"$maxCount bill(s) of $denominationProposed $currencyString\n"
          remainingAmount = 0
        } else {
          println(s"There is $remainingAmount $currencyString left to distribute.")
          println(s"You can get a maximum of $maxCount bill(s) of $denominationProposed CHF.")
          print(s"Type 'o' for OK or any other value less than the proposed one: ")
          val input = StdIn.readLine()
          if (input == "o") count = maxCount
          else if (input.toInt < maxCount) count = input.toInt
          if (count != 0) {
            withdrawalString += s"$count bill(s) of $denominationProposed $currencyString\n"
          }
          remainingAmount -= count * denominationProposed
          if (denominationProposed == 500 || denominationProposed == 50)
            denominationProposed = (denominationProposed / 2.5).toInt
          else denominationProposed = denominationProposed / 2
        }
      }

      println("Please withdraw the requested amount:")
      print(withdrawalString)

      // if it's euros
      if (currency == "2") {
        var wa2 = withdrawalAmount.toDouble
        wa2 *= 0.95
        balance -= wa2
      } else balance -= withdrawalAmount
      printf(s"Your withdrawal has been processed, the new amount available on your account is: %.2f\n",balance)
    } else if (choice == 3) {
      println(s"The amount available on your account is: $balance\n")
    } else if (choice == 4) {
      println("End of operations, don't forget to collect your card.")
      System.exit(0)
    } else println("Please enter a valid operation.\n")
  }
}