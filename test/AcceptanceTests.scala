import org.scalatest._
import org.scalatest.matchers._
import collection.mutable.ListBuffer

class RuleApiFeatureSpec extends FeatureSpec with GivenWhenThen with MustMatchers {

	val d1 = MapData(Map(("id","1"),("a","5foo"),("b","1234bar234")))
	val d2 = MapData(Map(("id","2"),("a","foo"),("b","bar45"))) 
	val d3 = MapData(Map(("id","3"),("a","454foo45"),("b","445bar")))
	val d4 = MapData(Map(("id","4"),("a","43foo45"),("b","bar87")))
	val d5 = MapData(Map(("id","5"),("a","foo45"),("b","3bar4546")))
	val d6 = MapData(Map(("id","6"),("a","cfoo"),("b","bar")))

	object MapDataSet extends DataSet {
		// in production, DataSet may come from a database (can be read one record at a time in foreach)
		val data = List(d1,d2,d3,d4,d5,d6)
	    def foreach(f:(Data)=>Unit):Unit = { data.foreach(f) }
	}

	val r0 = "f:b /^[0-9][0-9]+.*/ Convert x:transformAandB" // convert a and b if b starts with two numbers
	val r1 = "f:b /^[0-9]+.*/ Convert x:transformA" // convert a if b starts with a number
	val r2 = "f:a /^[0-9]+.*/ Filter" // filter if a starts with a number
	val r3 = "f:b /^b.*/ Convert x:transformB" // convert b if b starts with "b"
	val r4 = "f:a /^c.*/ NoMatch" // bad rule
	
	val rules = RuleList(List(r0,r1,r2,r3,r4),MyRuleParser)

	class ListOut extends Output {
	    var container = new ListBuffer[Data]()
	    def apply(dataIn:Data) = {
	       container+dataIn
	    }
		def getById(n:String):Data = { container.filter( (d) => d("id") == n )(0) } // assuming one match here
	    override def toString = "***OUTPUT***\n"+container.mkString("\n")
	}

	Log.setType("list")
	
	feature("Conversion API"){
		scenario("Filtered Output"){
			given("The default testing scenario set up above")
			when("I run the conversion")
			val out = new ListOut
			Conversion(MapDataSet,rules,out)
			then("I expect to see five records in the output")
			//println(Log.log)
			//println(out.container)
			out.container must have length 5
			then("The filtered record should be id 4")
			out.container.map( d => d("id") ).contains("4") must be === false
			then("The records not filtered would be 1,2,3,5,6")
			out.container.map( d => d("id") ).sameElements(List("1","2","3","5","6")) must be === true
		}
		scenario("Un-filtered Output"){
			given("The default testing scenario set up above")
			when("I run the conversion")
			val out = new ListOut
			Conversion(MapDataSet,rules,out)
			then("The 'a' field for ids 1 and 3 should have been converted")
			out.getById("1")("a") must be === "<NumA 5>foo"
			out.getById("3")("a") must be === "<NumA 454>foo45"
			then("The 'b' field for ids 1 and 2 should have been converted")
			out.getById("1")("b") must be === "1234bar<NumB 234>"
			out.getById("2")("b") must be === "bar<NumB 45>"
			then("The record with id 6 is unchanged")
			out.getById("6") must be === d6
		}
		scenario("Alternate Rules"){
			given("The default testing scenario with rule 0 out and a reset output")
			val rules = RuleList(List("f:b /^[0-9]+.*/ Filter",r1,r2,r3,r4),MyRuleParser)
			when("I run the conversion")
			val out = new ListOut
			Conversion(MapDataSet,rules,out)
			then("I expect to see two records in the output")
			out.container must have length 2
			then("The records not filtered would be 2,6")
			out.container.map( d => d("id") ).sameElements(List("2","6")) must be === true
			then("The 'b' field for id 2 should have been converted")
			out.getById("2")("b") must be === "bar<NumB 45>"
			then("The record with id 6 is unchanged")
			out.getById("6") must be === d6
		}
	}
}

class AcceptanceTestSuite extends Suite{ override def nestedSuites = List(new RuleApiFeatureSpec) }

