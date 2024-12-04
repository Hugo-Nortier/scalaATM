import scala.io.StdIn

object Exo2 extends App {
  val nbclients: Int = 100
  val accounts: Array[Double] = Array.fill(nbclients)(1200.0)
  val codespin: Array[String] = Array.fill(nbclients)("INTRO1234")
  var currrentUserID: Int = nbclients + 1 // get current user's login code
  var isFirstOperation = true // need to ask for login & pin codes ? 
  var choice = 0

  while (true) {
    while (isFirstOperation) { askForCodes() }
    println("Choose your operation:")
    println("1) Deposit")
    println("2) Withdrawal")
    println("3) Account consultation")
    println("4) Changing the pin code")
    println("5) Finish")
    print("Your choice > ")
    choice = StdIn.readInt()
    if (choice == 1) { deposit(currrentUserID, accounts) }
    if (choice == 2) { withdraw(currrentUserID, accounts) }
    if (choice == 3) { println("The amount available on your account is: "+accounts(currrentUserID))}
    if (choice == 4) { changepin(currrentUserID, codespin) }
    if (choice == 5) { println("End of operations, don't forget to collect your card.")
      isFirstOperation = true
    }
  }

  def askForCodes(): Unit = {
    // login code
    print("Enter your login code > ")
    val loginCode = StdIn.readInt()
    if (loginCode > nbclients) {
      println("This identifier is not valid.")
      System.exit(-1)
    } else currrentUserID = loginCode

    // pin code
    print("Enter your pin code > ")
    var remainingAttempts = 3
    var enteredPin = StdIn.readLine()
    while (enteredPin != codespin(currrentUserID) && remainingAttempts > 0) {
      remainingAttempts -= 1
      if (remainingAttempts > 0) {
        print(s"Wrong pin code, you still have $remainingAttempts attempts > ")
        enteredPin = StdIn.readLine()
      } else {
        println("Too many errors, abandonment of identification")
        isFirstOperation = true
        return
      }
    }
    isFirstOperation = false
  }

  def deposit(id: Int, accounts: Array[Double]): Unit = {
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
      print("Indicate the amount of the deposit > ")
      depositAmount = StdIn.readInt()
    }
    if (currency == "2") depositAmount *= 0.951
    this.accounts(id) += depositAmount
    println("Your deposit has been processed. The new amount available on your account is: "+accounts(id))
  }

  def withdraw(id: Int, accounts: Array[Double]): Unit = {
    val withdrawalLimit = accounts(id) * 0.10
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
      print("Indicate the amount of the withdrawal > ")
      withdrawalAmount = StdIn.readInt()
    }

    var denominationsType = ""

    if (withdrawalAmount < 200 || currency == "2") { // euro or withdraw < 200
      denominationsType = "2"
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
    } else { // large denominations & withdraw >= 200
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
      accounts(id) -= wa2
    } else accounts(id) -= withdrawalAmount
    printf(s"Your withdrawal has been processed, the new amount available on your account is: %.2f\n",accounts(id))
  }  

  def changepin(id: Int, codespin: Array[String]): Unit = {
    var newPinCode = ""
    while (newPinCode.length < 8) {
      print("Enter your new pin code (it must contain at least 8 characters) > ")
      newPinCode = StdIn.readLine()
      if (newPinCode.length < 8)
        println("Your pin code does not contain at least 8 characters")
    }
    codespin(id) = newPinCode
  }
}