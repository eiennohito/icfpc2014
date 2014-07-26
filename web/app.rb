require 'sinatra'

CR = "\015"

get '/' do
  haml :index, :format => :html4
end

post '/post' do
  @code = params[:code].gsub(/#{CR}/, "")
  @funcs = params[:funcs].gsub(/#{CR}/, "")

  @code_ = @funcs.split("\n").map { |func|
    "code ++= CodeGen.emitStructure(cleanAsts.get(\"#{func}\").get, gen)"
  }.join("\n")

  @class_code = <<-EOS
package jp.ac.kyotou.kansai

@gccCode
class AI extends Support {
#{@code}
}

object AI extends AstCleanup {
  val asts = ???

  def main(args: Array[String]) {
    var gen = NameGen()
    var code = List[Code]()
    #{@code_}
    code ++= List(Label("terminate"))
    println("<<<<<<<<<<")
    println(code.map(CodeGen.show).mkString("", "\\n", ""))
    println("----------")
    println(CodeGen.dereferenceLabels(code).map(CodeGen.show).mkString("", "\\n", ""))
    println(">>>>>>>>>>")
  }
}
  EOS

  File.open "../src/main/scala/jp/ac/kyotou/kansai/AI.scala", 'w' do |file|
    file.write @class_code
  end
  result = `cd ../; sbt run`

  if result.include?("error") then
    result = [result, ""]
  else
    result = result.split("<<<<<<<<<<")[1].split(">>>>>>>>>>")[0].split("----------")
  end
  haml :res, :locals => {:result => result}
end
