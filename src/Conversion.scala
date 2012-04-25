import util.parsing.combinator.RegexParsers
import util.matching.Regex
import collection.mutable.ListBuffer

/* This project is an experiment in constructing a data conversion architecture via a "DSL" like method. The primary functions are to use a set of "rules" to determine whether to "filter" or "convert" data in an input dataset. */

/* Abstract classes: concrete implementations of these can be used to create mocks for testing */

/* Data class represents a single data element. */

abstract class Data {
    def apply(a:String):String
    def has(a:String):Boolean
    def set(a:String,b:String):Data
    def isFiltered():Boolean
    def setFiltered():Data
}

abstract class DataSet {
    def foreach(f:(Data)=>Unit):Unit
}

trait RuleParser {
    def parse(in:String):Option[Rule]
}

/* Applying a RuleSet to a piece of data yields a new piece of data */

abstract class RuleSet {
    def apply(dataIn:Data):Data
}

/* Match functions are used to determine if a piece of data meets a particular criteria */

abstract class MatchFun {
    def apply(d:Data):Boolean
}

/* Convert functions take a piece of data and return a, possibly, different piece of data */

abstract class ConvertFun {
    def apply(d:Data):Data
}

/* Output object captures the resultant data */

abstract class Output {
    def apply(dataIn:Data)
}

abstract class Logger { def apply(s:String):Unit }

/* concrete implementations */

object Log {
    var log:Logger = StdOutLog // default: override with env variable
    def setType(t:String) = { t match { case "list" => log = ListLog; case "stdout" => log = StdOutLog } }
    object ListLog extends Logger {
        val container = new ListBuffer[String]();
        def apply(s:String):Unit = {
           container+s
        }
        override def toString = "***LOG***\n"+container.mkString("\n")
    }
    object StdOutLog extends Logger {
        def apply(s:String):Unit = {
           println(s)
        }
    }
}

trait Logging { def log(s:String) = { Log.log(s) } }

case class MapData(dataIn:Map[String,String]) extends Data { // using case class enables pattern matching
    val map = dataIn
    def apply(key:String):String = map(key)
    def has(key:String):Boolean = map.contains(key)
    def set(key:String,value:String):Data = MapData(map.update(key,value))
    def isFiltered():Boolean = map.contains("_status") && map("_status") == "filtered"
    def setFiltered():Data = MapData(map.update("_status","filtered"))
    override def toString = map.toString
}

object NoMatch extends MatchFun {
    def apply(dataIn:Data):Boolean = false
}

class PatternMatcher(f:String,p:Regex) extends MatchFun with Logging {
    def apply(dataIn:Data):Boolean = {
        val test = p.findFirstIn(dataIn(f))
        log("PatternMatcher pattern "+p+" testing "+f+" with val "+dataIn(f)+" and result is "+test)
        if( test == None ) false else true
    }
}

object NoConvert extends ConvertFun {
    def apply(dataIn:Data):Data = dataIn
}

object AMapper extends ConvertFun {
    def apply(dataIn:Data):Data = {
        dataIn.set("a", "^([0-9]+)".r.replaceFirstIn(dataIn("a"), "<NumA $1>"))
    }
}

object BMapper extends ConvertFun {
    def apply(dataIn:Data):Data = {
        dataIn.set("b", "^(.*?)([0-9]+)$".r.replaceFirstIn(dataIn("b"), "$1<NumB $2>"))
    }
}

object CMapper extends ConvertFun {
    def apply(dataIn:Data):Data = BMapper(AMapper(dataIn))
}

object ConvertFuns {
    def functions = Map[String,ConvertFun](
                        ("transformA", AMapper),
                        ("transformB", BMapper),
                        ("transformAandB", CMapper)
                      )
}

/* Rule takes a match function, and "action", and a convert Function when instantiated.
   When applied, the Rule takes a piece of data and returns a piece of data.  */

case class Rule(matcher:MatchFun, action:String, conversion:ConvertFun) extends Function[Data,Data] with Logging {
    def apply(dataIn:Data):Data = { 
        log("RULE: "+this.action+" on data "+dataIn+":")
		if(dataIn.isFiltered){ log("FILTERED"); dataIn }
		else if(matcher(dataIn)){
			action match {
				case "Filter" => { log("FILTERING"); dataIn.setFiltered() }
				case "Convert" => { log("CONVERTING"); conversion(dataIn) }
				case _ => { log("BAD ACTION"); dataIn }
			}
		}
		else{ log("NO MATCH"); dataIn }
    } 
} 

/* Parser takes a String and returns a Rule */

object MyRuleParser extends RegexParsers with RuleParser {
    def fieldStart:Parser[String] = "f:"
    def field:Parser[String] = "[A-Za-z]+".r ^^ { _.toString }
    def functionStart:Parser[String] = "x:"
    def function:Parser[String] = "[A-Za-z]+".r ^^ { _.toString }
    def pattern:Parser[Regex] = "/.*?/".r ^^ { case p => new Regex("/".r.replaceAllIn(p,"")) }
    def action:Parser[String] = "Filter|Convert".r ^^ { _.toString }
    def rule1:Parser[Rule] = fieldStart~field~pattern~action~functionStart~function ^^ { case fs~f~p~a~xs~x => Rule(new PatternMatcher(f,p),a,ConvertFuns.functions(x)) }
    def rule2:Parser[Rule] = fieldStart~field~pattern~action ^^ { case fs~f~p~a => Rule(new PatternMatcher(f,p),a,NoConvert) }
    def rule:Parser[Rule] = rule1 | rule2
    def parse(input:String):Option[Rule] = parseAll(rule,input) match {
        case Success(e,_) => Some(e)
        case f: NoSuccess => None
    }
}

/* Our RuleList (RuleSet) applies the individual Rules to a data input */

case class RuleList(rs:List[String],rp:RuleParser) extends RuleSet with Logging {
    val rules = rs.map { r => rp.parse(r) }.filter{ r => r != None }.map{ r => r.get }.toSeq
    def apply(dataIn:Data):Data = {
        log("APPLYING RULES")
        Function.chain(rules)(dataIn)
    }
}

/* The Conversion object represents the input point. It takes a DataSet, a RuleSet, and an Output. */

object Conversion extends Logging {
    def apply(data:DataSet, rules:RuleSet, output:Output) = {
        data.foreach(
            d => { 
                log("PRE PROCESSING: "+d)
                val o = rules(d)
                log("POST PROCESSING: "+o)
                if(!o.isFiltered()) { log("ADDING TO OUTPUT: "+o); output(o); }
            }
        )
    }
}

