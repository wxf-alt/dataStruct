package basics.structure

/**
 * @Auther: wxf
 * @Date: 2023/2/21 11:32:36
 * @Description: CalculatorDemo   实现计算器
 * @Version 1.0.0
 */
object CalculatorDemo {
  def main(args: Array[String]): Unit = {
    val calculator = new Calculator
    println(calculator.start("2*12-2/2-6/2-1"))
  }
}

/**
 * 表示一个计算器
 * 只考虑 +-*- 不考虑()等其他运算
 * 23 + 10 * 5 - 10
 */
class Calculator {

  /*
      计算给出的表达式的值: 利用栈的工作原理
      两个栈: 一个用来存放数字 另一个用来存放符号
   */
  def start(expression: String): Double = {
    // 数栈
    val numStack = new ArrayStack[Double](1000)
    // 符号栈
    val notationStack = new ArrayStack[Char](1000)
    // 1. 去掉字符串中可能的空白字符
    val expr: String = expression.replaceAll("\\s+", "");
    // 2. 遍历字符串, 存入到对应的栈中
    // 3. 进入栈的同时计算高优先级或者同等优先级的
    // 4. 把结果也压入栈中
    val sb = new StringBuilder // 用来缓存完整的数字(12)
    expr.foreach {
      // 如果是数字或者.(不是运算符), 则表示数字, 存入到 sb 中, 等将来碰到一个完整的数再压入到数栈中
      case c if c.toString.matches("[\\d\\.]") => sb.append(c)
      case c => { // 剩下情况就是运算符则: 1. sb 中的数字压入到数栈 2. 运算符压入到符号栈(先把优先级高的计算出来)

        numStack.push(sb.toDouble)
        sb.clear() // 清空缓存
        var isOver = false
        while (!isOver) { // 考虑到优先级高的算完之后, 出现优先级相同的连在一起, 所以要一直算下去
          // 比较新的运算符和栈顶的运算符的优先级: 如果低于或等于栈顶优先级, 则需要从把优先级高的计算出来, 然后把结果入栈
          val topNotation: Option[Char] = notationStack.peak()
          if (topNotation.isDefined && lteq(c, topNotation.get)) { // 栈顶有值, 并且 c 的运算符低于栈顶运算符的优先级, 则需要计算高优先级的运算
            val tmpRes: Double = calc(notationStack.pop().get, numStack.pop().get, numStack.pop().get)
            // 计算结果重新压入数栈
            numStack.push(tmpRes)
          } else {
            isOver = true
          }
        }
        notationStack.push(c) // c 运算符压入符号栈

      }
    }
    numStack.push(sb.toDouble) // 最后一个数字也要压如到栈中
    // 5. 计算栈中剩余优先级的运算
    while (!notationStack.isEmpty) {
      val tmpRes: Double = calc(notationStack.pop().get, numStack.pop().get, numStack.pop().get)
      numStack.push(tmpRes)
    }
    // 6. 返回结果
    numStack.pop().get
  }

  /**
   * 计算:  num2 op num1
   *
   * @param op
   * @param num1
   * @param num2
   */
  def calc(op: Char, num1: Double, num2: Double) = {
    op match {
      case '+' => num2 + num1
      case '-' => num2 - num1
      case '*' => num2 * num1
      case '/' => num2 / num1
    }
  }

  /**
   * 判断第一个运算符的优先级是否低于或等于第二个运算符的优先级
   *
   * @param notation1 第一个运算符
   * @param notation2 第二个运算符
   */
  def lteq(notation1: Char, notation2: Char): Boolean = getPriority(notation1) <= getPriority(notation2)

  /**
   * 定义运算符的优先级
   *
   * @param notation 优先级
   * @return
   */
  def getPriority(notation: Char): Int = {
    notation match {
      case '+' => 1
      case '-' => 1
      case '*' => 2
      case '/' => 2
    }
  }
} 
