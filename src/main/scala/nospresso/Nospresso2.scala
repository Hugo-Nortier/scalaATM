import scala.io.StdIn.{readInt, readLine, readDouble}

object Nospresso2 {
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    val enteredPin = readLine()
    return enteredPin == machinePins(machineId)
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var newPin = ""
    print(s"Updating PIN for Machine ${machineId+1}.\n")
    while (newPin.length != 6) {
      print("Enter new  6-digits PIN > ")
      newPin = readLine()    
    } 
    machinePins(machineId) = newPin
    println("PIN updated successfully.\nReturning to main menu...\n")
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var beverageName = ""
    var latteSize = ""
    var sugarLevel = ""
    var extraMilk = "No"

    var sugarUsed = 0
    var coffeePowderUsed = 0
    var milkUsed = 0
    var priceDisplayed = ""
    var price: Double = 0

    var beverage = 0
    while (beverage < 1 || beverage > 3) {
      println("Please select your beverage:")
      println("1) Espresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Small), CHF 3.20 (Medium), CHF 3.70 (Large)")
      print("> ")
      beverage = readInt()
    }

    if (beverage == 1) {
      beverageName = "Espresso"
      price = 2.00
      priceDisplayed = "CHF 2,00"
      coffeePowderUsed += 8
    } else if (beverage == 2) {
      beverageName = "Cappuccino"
      price = 2.50
      priceDisplayed = "CHF 2,50"
      coffeePowderUsed += 6
      milkUsed += 100
    } else if (beverage == 3) {
      beverageName = "Latte"
      var size = 0
      while (size < 1 || size > 3) {
        println("Please select the size for your Latte:")
        println("1) Small")
        println("2) Medium")
        println("3) Large")
        print("> ")
        size = readInt()
      }
      if (size == 1) {
        price = 2.70
        priceDisplayed = "CHF 2,70"
        coffeePowderUsed += 6
        milkUsed += 120
        latteSize = " (Small)"
      } else if (size == 2) {
        price = 3.20
        priceDisplayed = "CHF 3,20"
        coffeePowderUsed += 8
        milkUsed += 150
        latteSize = " (Medium)"
      } else if (size == 3) {
        price = 3.70
        priceDisplayed = "CHF 3,70"
        coffeePowderUsed += 12
        milkUsed += 200
        latteSize = " (Large)"
      }
    }

    var addSugar = 0
    while (addSugar < 1 || addSugar > 4) {
      println("Would you like to add sugar?")
      println("1) No sugar")
      println("2) Light (5g) - CHF 0.10")
      println("3) Medium (10g) - CHF 0.20")
      println("4) Heavy (15g) - CHF 0.30")
      print("> ")
      addSugar = readInt()
    }
    if (addSugar == 2) {
      price += 0.10
      priceDisplayed += " + CHF 0,10"
      sugarUsed += 5
      sugarLevel = "Light (5g)"
    } else if (addSugar == 3) {
      price += 0.20
      priceDisplayed += " + CHF 0,20"
      sugarUsed += 10
      sugarLevel = "Medium (10g)"
    } else if (addSugar == 4) {
      price += 0.30
      priceDisplayed += " + CHF 0,30"
      sugarUsed += 15
      sugarLevel = "Heavy (15g)"
    } else {
      sugarLevel = "No sugar"
    }

    if (beverage == 2 || beverage == 3) {
      var addExtraMilk = 0
      while (addExtraMilk < 1 || addExtraMilk > 2) {
        println("Would you like to add extra milk?")
        println("1) Yes")
        println("2) No")
        print("> ")
        addExtraMilk = readInt()
      }
      if (addExtraMilk == 1) {
        extraMilk = "Yes"
        var doses = 0
        while (doses < 1 || doses > 3) {
          println("How many doses?")
          print("> ")
          doses = readInt()
        }
        var extraMilkPrice = doses * 0.05
        price += extraMilkPrice
        priceDisplayed += " + CHF " + (f"$extraMilkPrice%.2f")
        milkUsed += doses * 5
      }
    }
    println("\nSelected beverage: " + beverageName + latteSize)
    println("Sugar level: " + sugarLevel)
    println("Extra milk: " + extraMilk)

    if (sugarUsed > sugarStocks(machineId)) {
      displayError("sugar")
    } else if (milkUsed > milkStocks(machineId)) {
      displayError("milk")
    } else if (coffeePowderUsed > coffeeStocks(machineId)) {
      displayError("coffee powder")
    } else {
      println(f"Total price: $priceDisplayed = CHF $price%.2f")
      println("\nPlease pay using Twint.")
      val characters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      var code = new StringBuilder
      for (x <- 1 to 5) code += characters((math.random() * 36).toInt)
      println("Your payment code is: " + code)
      println("(Waiting for payment validation...)")
      Thread.sleep(3000)
      println("Thank you! Your payment has been accepted.")
      coffeeStocks(machineId) -= coffeePowderUsed
      sugarStocks(machineId) -= sugarUsed
      milkStocks(machineId) -= milkUsed
      println("\nPreparing your beverage...")
      println("[...]")
      println("Your " + beverageName + " is ready! Enjoy!\n")
      return true
    }
    return false
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Current stock levels:")
    println("   Coffee powder : " + coffeeStocks(machineId) + "g")
    println("   Sugar         : " + sugarStocks(machineId) + "g")
    println(f"   Milk          : ${milkStocks(machineId) * 0.001}%.2f L")
    println("\nEnter quantities to add :")
    var additionCoffee = -1
    while (additionCoffee < 0) {
      print("   Coffee powder > ")
      additionCoffee = readInt()
    }
    var additionSugar = -1
    while (additionSugar < 0) {
      print("   Sugar         > ")
      additionSugar = readInt()
    }
    var additionMilk: Double = -1
    while (additionMilk < 0.0) {
      print("   Milk          > ")
      additionMilk = readDouble()
    }
    coffeeStocks(machineId) += additionCoffee
    milkStocks(machineId) += (additionMilk * 1000).toInt
    sugarStocks(machineId) += additionSugar
    println(s"Stocks updated successfully for Machine ${machineId+1}.")
    println("Returning to the main menu...\n")
  }

  def displayError(ingredient: String): Unit = {
    println(s"\nError: Insufficient $ingredient to prepare the selected beverage.")
    println("Please select another machine.")
  }

  def main(args: Array[String]): Unit = {
    val nbMachines = 5
    val coffeeStocks = Array.fill(nbMachines)(50)
    val sugarStocks = Array.fill(nbMachines)(30)
    val milkStocks = Array.fill(nbMachines)(500)
    val machinePins = Array.fill(nbMachines)("434343")

    var mode = 0

    while (mode != 3) {
      println("        Nospresso Cafe")
      while (mode < 1 || mode > 3) {
        println("Please select your mode:")
        println("1) Client")
        println("2) Admin")
        println("3) Exit")
        print("> ")
        mode = readInt()
      }

      if (mode == 3) return

      var selectedMachine = 0
      while (selectedMachine < 1 || selectedMachine > 5) {
        println("Selected machine (1-5) >")
        selectedMachine = readInt()
      }
      val machineId = selectedMachine - 1

      if (mode == 1) {
        serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
        mode = 0
      } else if (mode == 2) {
        println(s"Enter PIN for Machine $selectedMachine")
        var remainingAttempts = 3
        while (remainingAttempts > 0) {
          if (!validatePin(machineId, machinePins)) {
            remainingAttempts -= 1
            println(s"Incorrect PIN. $remainingAttempts attempts remaining.")
            if (remainingAttempts == 0) {
              println("Too many failed attempts. Exiting program.")
              return
            }
          } else {
            println(s"Access granted to Machine $selectedMachine.")
            remainingAttempts = -1
          }
        }
        var adminChoice = 0
        while (adminChoice != 1 && adminChoice != 2) {
          println("Choose your operation:")
          println("1) Restocking Ingredients")
          println("2) Updating PIN")
          adminChoice = readInt()
        }
        if (adminChoice == 1) {
          restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
        }
        if (adminChoice == 2) {
          updatePin(machineId, machinePins)
        }
        mode = 0
      }
    }
  }
}