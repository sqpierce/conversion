import org.scalatest._
import org.scalatest.matchers._
import util.matching.Regex

class MapDataSpec extends FlatSpec {
	val m = MapData(Map(("id","1")))
	"A MapData object with an 'id' field" should "have an 'id' key" in {
		expect(true){ m.has("id") }
	}
	"A MapData object with an 'id' field with value '1'" should "return that value for that key" in {
		expect("1"){ m("id") }
	}
	"A MapData object which has no 'a' field set" should "not recognize key and throw error when accessed" in {
		expect(false){ m.has("a") }
		intercept[NoSuchElementException]{ m("a") }
	}
	"A MapData object which has a field set" should "recognize that key and return that value for that key" in {
		val m2 = m.set("a", "foo")
		expect(true){ m2.has("a") }
		expect("foo"){ m2("a") }
	}
	"A MapData object which has not had filtered set" should "return false for isFiltered" in {
		expect(false){ m.isFiltered }
	}
	"A MapData object which has filtered set" should "return true for isFiltered" in {
		val m2 = m.setFiltered
		expect(true){ m2.isFiltered }
	}
}

class MatchFunSpec extends FlatSpec {
	val m = MapData(Map(("id","1"),("a","123foo345"),("b","432blah7898")))
	"Data passed to NoMatch" should "always return false" in {
		expect(false){ NoMatch(m) }
		expect(false){ NoMatch(m.setFiltered) }
		expect(false){ NoMatch(m.set("id","5")) }
		expect(false){ NoMatch(m.set("a","foo")) }
		expect(false){ NoMatch(m.set("b","bar")) }
	}	
	"Data passed to a PatternMatch" should "return true or false based on regex and field given" in {
		expect(true){ new PatternMatcher("id","^[0-9]+$".r)(m) }
		expect(false){ new PatternMatcher("id","[a-z]".r)(m) }
		expect(true){ new PatternMatcher("a","^123".r)(m) }
		expect(false){ new PatternMatcher("a","35$".r)(m) }
		expect(true){ new PatternMatcher("b","blah".r)(m) }
		expect(false){ new PatternMatcher("b","bar".r)(m) }
	}	
}

class ConvertFunSpec extends FlatSpec {
	val m = MapData(Map(("id","1"),("a","123foo345"),("b","432blah7898")))
	"Data passed to NoConvert" should "not be altered" in {
		expect(true){ m == NoConvert(m) }
	}
	"Data with an 'a' value that starts with a num passed to AMapper" should "have its 'a' field altered" in {
		expect("123foo345"){ m("a") }
		expect("<NumA 123>foo345"){ AMapper(m)("a") }
	}
	"Data with an 'b' value that ends with a num passed to BMapper" should "have its 'b' field altered" in {
		expect("432blah7898"){ m("b") }
		expect("432blah<NumB 7898>"){ BMapper(m)("b") }
	}
	"Data with appropriate 'a'&'b' values passed CMapper" should "have its fields altered" in {
		expect("123foo345"){ m("a") }
		expect("432blah7898"){ m("b") }
		expect("<NumA 123>foo345"){ CMapper(m)("a") }
		expect("432blah<NumB 7898>"){ CMapper(m)("b") }
	}
}

class UnitTestSuite extends Suite{ override def nestedSuites = List(new MapDataSpec,new MatchFunSpec,new ConvertFunSpec) }

