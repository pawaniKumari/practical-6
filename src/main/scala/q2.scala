object StudentRecords{
    import scala.io.StdIn.readLine

    def getStudentInfo() : (String, Int, Int) = {
        val name = readLine("Enter student's name:")
        val marks = readLine("Enter student's marks:").toInt
        val totMarks = readLine("Enter student's total possible marks:").toInt

        (name, marks, totMarks)
    }

    def printStudentRecord(student: (String, Int, Int, Double, Char)) : Unit = {
        val (name, marks, totMarks, percentage, grade) = student
        println(s"Name : $name")
        println(s"Marks : $marks/$totMarks")
        println(s"Percentage : $percentage")
        println(s"Grade : $grade")
    }

    def validateInput(name:String, marks:Int, totMarks:Int) : (Boolean, Option[String]) = {
        if (name.isEmpty) {
            (false, Some("Name cannot be empty"))
        }else if (marks < 0 || marks > totMarks) {
            (false, Some("Marks should be positive and can't exceed total marks"))
        }else {
            (true, None)
        }
    }

    def getStudentInfoWithRetry() : (String, Int, Int, Double, Char) = {
        var validInput = false
        var studentInfo : (String,Int, Int, Double, Char) = ("", 0, 0, 0.0, 'F')

        while (!validInput) {
           var (name, marks,totalMarks) = getStudentInfo()

            val(isValid, errorMessage) = validateInput(name, marks, totalMarks)
            if(isValid){
                val percentage = (marks.toDouble / totalMarks) * 100
                val grade = percentage match{
                    case a if a >= 90 => 'A'
                    case a if a >= 75 => 'B'
                    case a if a >= 50 => 'C'
                    case _ => 'D'
                }
                studentInfo = (name, marks, totalMarks, percentage, grade)
                validInput = true
            } else {
                println(s"Invalid input : ${errorMessage.get}")
            }
        }
        studentInfo
    }
}

object Main extends App{
    val student = StudentRecords.getStudentInfoWithRetry()
    StudentRecords.printStudentRecord(student)
}
