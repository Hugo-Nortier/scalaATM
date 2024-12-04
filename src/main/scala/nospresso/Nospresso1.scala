import scala.io.StdIn.{readInt, readDouble}

object Nospresso {
  def main(args: Array[String]): Unit = {
    // stock:
    var sugar = 30
    var coffeePowder = 50
    var milk: Double = 0.5

    var mode = 0
    while (mode != 3) {
      while (mode < 1 || mode > 3) {
        println("        Nospresso Cafe")
        println("Please select your mode:")
        println("1) Client")
        println("2) Admin")
        println("3) Exit")
        print("> ")
        mode = readInt()
      }

      if (mode == 1) { // Client Mode
        var beverageName = ""
        var latteSize = ""
        var sugarLevel = ""
        var extraMilk = "No"

        var sugarUsed = 0
        var coffeePowderUsed = 0
        var milkUsed: Double = 0
        var priceDisplayed= ""
        var price : Double = 0

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
          priceDisplayed= "CHF 2,00"
          coffeePowderUsed += 8
        } else if (beverage == 2) {
          beverageName = "Cappuccino"
          price = 2.50
          priceDisplayed= "CHF 2,50"
          coffeePowderUsed += 6
          milkUsed += 0.1
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
            priceDisplayed= "CHF 2,70"
            coffeePowderUsed += 6
            milkUsed += 0.12
            latteSize = " (Small)"
          } else if (size == 2) {
            price = 3.20
            priceDisplayed= "CHF 3,20"
            coffeePowderUsed += 8
            milkUsed += 0.15
            latteSize = " (Medium)"
          } else if (size == 3) {
            price = 3.70
            priceDisplayed= "CHF 3,70"
            coffeePowderUsed += 12
            milkUsed += 0.2
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
          priceDisplayed+= " + CHF 0,10"
          sugarUsed += 5
          sugarLevel = "Light (5g)"
        } else if (addSugar == 3) {
          price += 0.20
          priceDisplayed+= " + CHF 0,20"
          sugarUsed += 10
          sugarLevel = "Medium (10g)"
        } else if (addSugar == 4) {
          price += 0.30
          priceDisplayed+= " + CHF 0,30"
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
            priceDisplayed+= " + CHF " +  (f"$extraMilkPrice%.2f")
            milkUsed += doses * 0.05
          }
        }
        println("\nSelected beverage: " + beverageName + latteSize)
        println("Sugar level: " + sugarLevel)
        println("Extra milk: " + extraMilk)

        if (sugarUsed > sugar) {
          println("\nError: Insufficient sugar to prepare the selected beverage.")
          println("Please choose another beverage or check stocks in Admin mode.")
        } else if (milkUsed > milk) {
          println("\nError: Insufficient milk to prepare the selected beverage.")
          println("Please choose another beverage or check stocks in Admin mode.")
        } else if (coffeePowderUsed > coffeePowder) {
          println("\nError: Insufficient coffee powder to prepare the selected beverage.")
          println("Please choose another beverage or check stocks in Admin mode.")
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
          coffeePowder -= coffeePowderUsed
          sugar -= sugarUsed
          milk -= milkUsed
          println("\nPreparing your beverage...")
          println("[...]")
          println("Your " + beverageName + " is ready! Enjoy!\n")
        }
        mode = 0
      } 
      if (mode == 2) { // Admin Mode
        val adminPIN = 434343
        println("Admin Mode")
        var PIN = 0
        while (PIN != adminPIN) {
          print("Enter PIN: ")
          PIN = readInt()
        }
        println("Access granted.")
        println("Stocks:")
        println("   Coffee powder: " + coffeePowder)
        println("   Milk         : " + milk)
        println("   Sugar        : " + sugar)
        println("Replenishing stocks...")
        println("Addition: ")
        var additionCoffee = 0
        print("   Coffee powder: ")
        additionCoffee = readInt()
        var additionMilk: Double = 0
        print("   Milk         : ")
        additionMilk = readDouble()
        var additionSugar = 0
        print("   Sugar        : ")
        additionSugar = readInt()

        coffeePowder += additionCoffee
        milk += additionMilk
        sugar += additionSugar
        println("Stock levels updated.")
        println("Returning to the main menu...\n")
        mode = 0
      }
    }
  }
}