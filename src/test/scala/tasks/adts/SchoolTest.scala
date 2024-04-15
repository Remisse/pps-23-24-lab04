package tasks.adts

import org.junit.*
import org.junit.Assert.*
import tasks.adts.SchoolModel.*
import u03.Optionals.*
import u03.Sequences.*

class SchoolTest:

  val schoolADT: SchoolModule = BasicSchoolADT
  import schoolADT.*

  val courseName = "c1"
  val teacherName = "t1"

  @Test def testAddTeacher(): Unit =
    val teacher = school().addTeacher(teacherName).teacherByName(teacherName)
    assertNotEquals(nilTeacher(), Optional.orElse(teacher, nilTeacher()))

  @Test def testAddCourse(): Unit =
    val course = school().addCourse(courseName).courseByName(courseName)
    assertNotEquals(nilCourse(), Optional.orElse(course, nilCourse()))

  @Test def testAddTeacherToCourse(): Unit =
    var s = school().addCourse(courseName).addTeacher(teacherName)
    val course = Optional.orElse(s.courseByName(courseName), nilCourse())
    var teacher = Optional.orElse(s.teacherByName(teacherName), nilTeacher())
    s = s.setTeacherToCourse(teacher, course)
    teacher = Optional.orElse(s.teacherByName(teacherName), nilTeacher())
    assertEquals(Sequence.Cons(course, Sequence.Nil()), s.coursesOfATeacher(teacher))
