defmodule Giftex.ParserTest do
  use ExUnit.Case
  alias Giftex.Parser

  setup_all do
    :ok = Parser.build
    IEx.Helpers.c("src/gift.erl")
    :ok
  end

  def parse_first(str) do
    [h | _] = :gift.parse(str)
    h
  end

  test "description question" do
    assert %Gift.Description{text: "This is a description."} = parse_first("This is a description.")
  end

  test "essay question" do
    assert %Gift.EssayQuestion{text: "Write an essay about something."} = parse_first("Write an essay about something.{}")
  end

  test "true false question" do
    assert %Gift.TrueFalseQuestion{text: "The sky is blue.", answers: [{:true, 100}]} = parse_first("The sky is blue.{T}")
    assert %Gift.TrueFalseQuestion{text: "The sky is blue.", answers: [{:true, 100}]} = parse_first("The sky is blue.{TRUE}")
    assert %Gift.TrueFalseQuestion{text: "The sky is green.", answers: [{:false, 100}]} = parse_first("The sky is green.{F}")
    assert %Gift.TrueFalseQuestion{text: "The sky is green.", answers: [{:false, 100}]} = parse_first("The sky is green.{FALSE}")
  end

  test "feedback on question" do
    p = parse_first("Grant is buried in Grant's tomb.{FALSE#No one is buried in Grant's tomb.}")
    assert %Gift.TrueFalseQuestion{text: "Grant is buried in Grant's tomb.",
                                   answers: [{false, 100, "No one is buried in Grant's tomb."}]} = p
  end

  test "multiple choice question" do
    p = parse_first("What color is the sky?{ =Blue ~Green ~Red}")
    assert %Gift.MultipleChoiceQuestion{text: "What color is the sky?",
                      answers: [{"Blue", 100}, {"Green", 0}, {"Red", 0}]} = p
  end

  test "multiple choice question with feedback" do
    p = parse_first "What color is the sky?{ = Blue#Right ~Green ~Red#Very wrong}"
    assert %Gift.MultipleChoiceQuestion{text: "What color is the sky?",
                                        answers: [{"Blue", 100, "Right"},
                                                  {"Green", 0},
                                                  {"Red", 0, "Very wrong"}]} = p
  end

  test "multiple choice on multiple lines" do
    p = parse_first "What color is the sky?{\n= Blue#Right\n~Green\n~Red\n#Very wrong}\n"
    assert %Gift.MultipleChoiceQuestion{text: "What color is the sky?",
                                        answers: [{"Blue", 100, "Right"},
                                                  {"Green", 0},
                                                  {"Red", 0, "Very wrong"}]} = p
  end

  test "multiple choice question with weight" do
    p = parse_first "Which of these are primary colors?{ ~%33%Blue ~%33%Yellow ~Beige ~%33.3%Red}"
    assert %Gift.MultipleChoiceQuestion{text: "Which of these are primary colors?",
                                        answers: [{"Blue", 33},
                                                  {"Yellow", 33},
                                                  {"Beige", 0},
                                                  {"Red", 33.3}]} = p
  end

  test "numeric question with tolerance" do
    p = parse_first "How many pounds in a kilogram?{#2.2:0.1}"

    assert %Gift.NumericQuestion{answers: [{{2.1,2.3000000000000003}, 100}], text: "How many pounds in a kilogram?"} = p
  end

  test "test numeric with multiple answers" do
    p = parse_first "What is the value of PI?{#3.1415 =%50%3.1 =%25%3 }"

    assert %Gift.NumericQuestion{answers: [{3.1415, 100}, {3.1, 50}, {3, 25}]} = p
  end

  test "test numeric negative numbers" do
    p = parse_first "Calculate 2 - 6.{#-4.0}"
    assert %Gift.NumericQuestion{answers: [{-4.0, 100}]} = p
  end

  test "test numeric range question" do
    p = parse_first "::Q5:: What is a number from 1 to 5? {#1..5}"
    assert %Gift.NumericQuestion{answers: [{{1,5}, 100}]} = p
  end

  test "short answer question" do
    p = parse_first "Who's buried in Grant's tomb?{=Grant =Ulysses S. Grant =Ulysses Grant}"
    assert %Gift.ShortAnswerQuestion{text: "Who's buried in Grant's tomb?",
                                     answers: [{"Grant", 100}, {"Ulysses S. Grant", 100}, {"Ulysses Grant", 100}]} = p
  end

  test "matching question" do
    p = parse_first """
    Match the following countries with their corresponding capitals. {
           =Canada -> Ottawa
           =Italy  -> Rome
           =Japan  -> Tokyo
           }
"""

  assert %Gift.MatchingQuestion{text: "Match the following countries with their corresponding capitals.",
                                answers: [{"Canada", "Ottawa"}, {"Italy", "Rome"}, {"Japan", "Tokyo"}]} = p
  end

  test "fill in question" do
    p = parse_first "Little {~blue =red ~green } riding hood.\n"
    assert %Gift.FillInQuestion{text: "Little _ riding hood.", answers: [{"blue", 0}, {"red", 100}, {"green", 0}]} = p
  end

  test "fill in question two" do
    p = parse_first "Two plus two equals {=four =4}."
    assert %Gift.FillInQuestion{text: "Two plus two equals _ .", answers: [{"four", 100}, {"4",100}]} = p
  end

  test "question with title" do
    p = parse_first "::Colors 1:: Which of these are primary colors?{ ~%33%Blue ~%33%Yellow ~Beige ~%33%Red}"
    assert %Gift.MultipleChoiceQuestion{title: "Colors 1"} = p
  end

  test "question with comment" do
    p = parse_first("//This is an easy one\n Which of these are primary colors?{ ~%33%Blue ~%33%Yellow ~Beige ~%33%Red}")
    assert  %Gift.MultipleChoiceQuestion{text: "Which of these are primary colors?"} = p
  end

  test "multiline comments" do
    p = parse_first "//This is an easy one\n//With more than one line of comment\nWhich of these are primary colors?{ ~%33%Blue ~%33%Yellow ~Beige ~%33%Red}"
    assert  %Gift.MultipleChoiceQuestion{text: "Which of these are primary colors?"} = p
  end

  test "questions must be separated by a line" do
    test_text = """
        Who's buried in Grant's tomb?{=Grant =Ulysses S. Grant =Ulysses Grant}
        Which of these are primary colors?{ ~%33%Blue ~%33%Yellow ~Beige ~%33%Red}

"""
    assert {[%Gift.ShortAnswerQuestion{}],_, {{:line, 2}, {:column, 9}}} = :gift.parse(test_text)
  end

  test "questions separated by a line" do
  test_text = """
      Who's buried in Grant's tomb?{=Grant =Ulysses S. Grant =Ulysses Grant}

      Which of these are primary colors?{~%33%Blue ~%33%Yellow ~Beige ~%33%Red}
"""

    assert [%Gift.ShortAnswerQuestion{}, %Gift.MultipleChoiceQuestion{}] = :gift.parse(test_text)
  end

  test "escape left bracket" do
    p = :gift.parse("Can a \\{ be escaped?{=Yes a \\{ can ~No}")
    assert [%Gift.MultipleChoiceQuestion{answers: [{"Yes a { can", 100}, {"No", 0}], text: "Can a { be escaped?", title: ""}] = p
  end

  test "escape right bracket" do
    p = :gift.parse("Can a \\} be escaped?{=Yes a \\} can ~No}")
    assert [%Gift.MultipleChoiceQuestion{answers: [{"Yes a } can", 100}, {"No", 0}], text: "Can a } be escaped?", title: ""}] = p
  end

  test "escape colon" do
    p = :gift.parse("Can a \\: be escaped?{=Yes a \\: can ~No}")
    assert [%Gift.MultipleChoiceQuestion{answers: [{"Yes a : can", 100}, {"No", 0}], text: "Can a : be escaped?", title: ""}] = p
  end

  test "escape hash" do
    p = :gift.parse("Can a \\# be escaped?{=Yes a \\# can ~No}")
    assert [%Gift.MultipleChoiceQuestion{answers: [{"Yes a # can", 100}, {"No", 0}], text: "Can a # be escaped?", title: ""}] = p
  end

  test "escape tilde" do
    p = :gift.parse("Can a \\~ be escaped?{=Yes a \\~ can ~No}")
    assert [%Gift.MultipleChoiceQuestion{answers: [{"Yes a ~ can", 100}, {"No", 0}], text: "Can a ~ be escaped?", title: ""}] = p
  end

  test "escape equals" do
    p = :gift.parse("Can a \\= be escaped?{=Yes a \\= can ~No}")
    assert [%Gift.MultipleChoiceQuestion{answers: [{"Yes a = can", 100}, {"No", 0}], text: "Can a = be escaped?", title: ""}] = p
  end

  test "escapes in a comment" do
    p = parse_first("//Escapes in comments are redundant \\: since they end in a \n Question?{}")
    assert %Gift.EssayQuestion{text: "Question?"} = p
  end

  test "crlf line breaks" do
    p = :gift.parse "Can we have DOS style line breaks?{\r\n=yes \r\n~no}\r\n \r\n And is it seen as a line_break?{TRUE}"
    assert [%Gift.MultipleChoiceQuestion{}, %Gift.TrueFalseQuestion{}] = p
  end

  test "can have line breaks after comments" do
    p = :gift.parse "//Comment\nWho's buried in Grant's tomb?{=Grant =Ulysses S. Grant =Ulysses Grant}"
    assert [%Gift.ShortAnswerQuestion{}] = p
  end

  test "dealing with blank lines at start" do
    p = :gift.parse "\n    \n//Comment\nWho's buried in Grant's tomb?{=Grant =Ulysses S. Grant =Ulysses Grant}"
    assert [%Gift.ShortAnswerQuestion{}] = p
  end

  test "dealing with blank lines at end" do
    p = :gift.parse "//Comment\nWho's buried in Grant's tomb?{=Grant =Ulysses S. Grant =Ulysses Grant}\n \n    \n "
    assert [%Gift.ShortAnswerQuestion{}] = p
  end

  test "title line breaks" do
    p = :gift.parse """
     :: Title ::
     Match the following countries with their corresponding capitals.{=Canada -> Ottawa
     =Italy  -> Rome
     =Japan  -> Tokyo
     =India  -> New Delhi
     }
  """
    assert [%Gift.MatchingQuestion{title: "Title"}] = p
  end

  test "single short answer with comment" do
    p = :gift.parse """
    // ===Short Answer===
    What is your favorite color?{=blue}
"""
    assert [%Gift.ShortAnswerQuestion{}] = p
  end

  test "single command" do
    p = :gift.parse("$COMMAND=1\n\nQuestion{}\n\n")

    assert [%Gift.Command{command: "COMMAND=1"}, %Gift.EssayQuestion{text: "Question"}] = p
  end

  test "multiple commands" do
    p = :gift.parse("$CATEGORY=food\n$usecase=false\nQuestion{}\n\n")
    assert [%Gift.Command{command: "CATEGORY=food"}, %Gift.Command{command: "usecase=false"}, %Gift.EssayQuestion{text: "Question"}] = p
  end

  test "markup type" do
    p = parse_first "[textile] This *essay* is marked up in textile.{}\n\n"
    assert %Gift.EssayQuestion{text: "This *essay* is marked up in textile.", markup_language: :textile} = p
  end

  test "markup and title defaults" do
    p = parse_first "This essay is in plain text.{}\n\n"
    assert %Gift.EssayQuestion{text: "This essay is in plain text.", markup_language: :plain, title: ""} = p
  end
end
