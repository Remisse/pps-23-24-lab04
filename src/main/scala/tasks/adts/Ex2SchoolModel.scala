package tasks.adts
import u03.Sequences.*
import u03.Optionals.*

/*  Exercise 2: 
 *  Implement the below trait, and write a meaningful test.
 *  Suggestion: 
 *  - reuse Sequences and Optionals as imported above
 *  - Course is a simple case classes with just the name
 *  - Teacher is a case class with name and sequence of courses
 *  - School is a case class with (sequences of) teachers and courses
 *  - add/set methods below create the new school 
 */

object SchoolModel:

  trait SchoolModule:
    type School
    type Teacher
    type Course

    def school(): School
    def nilTeacher(): Teacher
    def nilCourse(): Course
    extension (school: School)
      def addTeacher(name: String): School
      def addCourse(name: String): School
      def teacherByName(name: String): Optional[Teacher]
      def courseByName(name: String): Optional[Course]
      def nameOfTeacher(teacher: Teacher): String
      def nameOfCourse(course: Course): String
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]

  object BasicSchoolADT extends SchoolModule:

    private case class SchoolImpl(teachers: Sequence[Teacher], courses: Sequence[Course])
    opaque type School = SchoolImpl
    private case class TeacherImpl(name: String, courses: Sequence[Course])
    opaque type Teacher = TeacherImpl
    private case class CourseImpl(name: String)
    opaque type Course = CourseImpl

    def school(): School = SchoolImpl(Sequence.Nil(), Sequence.Nil())
    def nilTeacher(): Teacher = TeacherImpl("", Sequence.Nil())
    def nilCourse(): Course = CourseImpl("")
    extension (school: School)
      def addTeacher(name: String): School = 
        SchoolImpl(Sequence.Cons(TeacherImpl(name, Sequence.Nil()), school.teachers), school.courses)
      def addCourse(name: String): School = 
        SchoolImpl(school.teachers, Sequence.Cons(CourseImpl(name), school.courses))

      def teacherByName(name: String): Optional[Teacher] = findFirst(school.teachers, t => t.name == name)
      def courseByName(name: String): Optional[Course] = findFirst(school.courses, c => c.name == name)

      def nameOfTeacher(teacher: Teacher): String = teacher.name
      def nameOfCourse(course: Course): String = course.name

      def setTeacherToCourse(teacher: Teacher, course: Course): School =
        val m: Teacher => Teacher = t => if t.name == teacher.name then TeacherImpl(t.name, Sequence.Cons(course, t.courses)) else t
        SchoolImpl(Sequence.map(school.teachers)(m), school.courses)

      def coursesOfATeacher(teacher: Teacher): Sequence[Course] = teacher.courses

      private def findFirst[A](s1: Sequence[A], pred: A => Boolean): Optional[A] = Sequence.filter(s1)(pred) match
        case Sequence.Cons(h, _) => Optional.Just(h)
        case _ => Optional.Empty()
