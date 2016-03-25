defmodule Giftex.ParserTest do
  use ExUnit.Case

  setup_all do
    :ok = Giftex.build_parser
    IEx.Helpers.c("src/gift.erl")
    :ok
  end

  def parse_first(str) do
    [h | _] = :gift.parse(str)
    h
  end

  test "description question" do
    assert %{type: :description, text: "This is a description."} = parse_first("This is a description.\n\n")
  end

  test "essay question" do
    assert %{ type: :essay_question,  text: "Write an essay about something."} = parse_first("Write an essay about something.{}")
  end

  test "true false question" do
    assert %{ type: :true_false_question,  text: "The sky is blue.", answers: [{:true, 100}]} = parse_first("The sky is blue.{T}")
    assert %{ type: :true_false_question,  text: "The sky is blue.", answers: [{:true, 100}]} = parse_first("The sky is blue.{TRUE}")
    assert %{ type: :true_false_question,  text: "The sky is green.", answers: [{:false, 100}]} = parse_first("The sky is green.{F}")
    assert %{ type: :true_false_question,  text: "The sky is green.", answers: [{:false, 100}]} = parse_first("The sky is green.{FALSE}")
  end

  test "feedback on question" do
    p = parse_first("Grant is buried in Grant's tomb.{FALSE#No one is buried in Grant's tomb.}")
    assert %{ type: :true_false_question,  text: "Grant is buried in Grant's tomb.",
                                   answers: [{false, 100, "No one is buried in Grant's tomb."}]} = p
  end

  test "multiple choice question" do
    p = parse_first("What color is the sky?{ =Blue ~Green ~Red}")
    assert %{type: :multiple_choice_question,  text: "What color is the sky?",
                      answers: [{"Blue", 100}, {"Green", 0}, {"Red", 0}]} = p
  end

  test "multiple choice question with feedback" do
    p = parse_first "What color is the sky?{ = Blue#Right ~Green ~Red#Very wrong}"
    assert %{type: :multiple_choice_question,  text: "What color is the sky?",
                                        answers: [{"Blue", 100, "Right"},
                                                  {"Green", 0},
                                                  {"Red", 0, "Very wrong"}]} = p
  end

  test "multiple choice on multiple lines" do
    p = parse_first "What color is the sky?{\n= Blue#Right\n~Green\n~Red\n#Very wrong}\n"
    assert %{type: :multiple_choice_question,  text: "What color is the sky?",
                                        answers: [{"Blue", 100, "Right"},
                                                  {"Green", 0},
                                                  {"Red", 0, "Very wrong"}]} = p
  end

  test "multiple choice question with weight" do
    p = parse_first "Which of these are primary colors?{ ~%33%Blue ~%33%Yellow ~Beige ~%33.3%Red}"
    assert %{type: :multiple_choice_question,  text: "Which of these are primary colors?",
                                        answers: [{"Blue", 33},
                                                  {"Yellow", 33},
                                                  {"Beige", 0},
                                                  {"Red", 33.3}]} = p
  end

  test "numeric question with tolerance" do
    p = parse_first "How many pounds in a kilogram?{#2.2:0.1}"

    assert %{type: :numeric_question,  answers: [{{2.1,2.3000000000000003}, 100}], text: "How many pounds in a kilogram?"} = p
  end

  test "test numeric with multiple answers" do
    p = parse_first "What is the value of PI?{#3.1415 =%50%3.1 =%25%3 }"

    assert %{type: :numeric_question,  answers: [{3.1415, 100}, {3.1, 50}, {3, 25}]} = p
  end

  test "test numeric negative numbers" do
    p = parse_first "Calculate 2 - 6.{#-4.0}"
    assert %{type: :numeric_question,  answers: [{-4.0, 100}]} = p
  end

  test "test numeric range question" do
    p = parse_first "::Q5:: What is a number from 1 to 5? {#1..5}"
    assert %{type: :numeric_question,  answers: [{{1,5}, 100}]} = p
  end

  test "short answer question" do
    p = parse_first "Who's buried in Grant's tomb?{=Grant =Ulysses S. Grant =Ulysses Grant}"
    assert %{type: :short_answer_question,  text: "Who's buried in Grant's tomb?",
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

  assert %{type: :matching_question,  text: "Match the following countries with their corresponding capitals.",
                                answers: [{"Canada", "Ottawa"}, {"Italy", "Rome"}, {"Japan", "Tokyo"}]} = p
  end

  test "fill in question" do
    p = parse_first "Little {~blue =red ~green } riding hood.\n"
    assert %{type: :fill_in_question,  text: "Little _ riding hood.", answers: [{"blue", 0}, {"red", 100}, {"green", 0}]} = p
  end

  test "fill in question two" do
    p = parse_first "Two plus two equals {=four =4}."
    assert %{type: :fill_in_question,  text: "Two plus two equals _ .", answers: [{"four", 100}, {"4",100}]} = p
  end

  test "question with title" do
    p = parse_first "::Colors 1:: Which of these are primary colors?{ ~%33%Blue ~%33%Yellow ~Beige ~%33%Red}"
    assert %{type: :multiple_choice_question,  title: "Colors 1"} = p
  end

  test "question with comment" do
    p = parse_first("//This is an easy one\n Which of these are primary colors?{ ~%33%Blue ~%33%Yellow ~Beige ~%33%Red}")
    assert  %{type: :multiple_choice_question,  text: "Which of these are primary colors?"} = p
  end

  test "multiline comments" do
    p = parse_first "//This is an easy one\n//With more than one line of comment\nWhich of these are primary colors?{ ~%33%Blue ~%33%Yellow ~Beige ~%33%Red}"
    assert  %{type: :multiple_choice_question,  text: "Which of these are primary colors?"} = p
  end

  test "questions must be separated by a line" do
    test_text = """
        Who's buried in Grant's tomb?{=Grant =Ulysses S. Grant =Ulysses Grant}
        Which of these are primary colors?{ ~%33%Blue ~%33%Yellow ~Beige ~%33%Red}

"""
    assert {[%{type: :short_answer_question,  }],_, {{:line, 2}, {:column, 9}}} = :gift.parse(test_text)
  end

  test "questions separated by a line" do
  test_text = """
      Who's buried in Grant's tomb?{=Grant =Ulysses S. Grant =Ulysses Grant}

      Which of these are primary colors?{~%33%Blue ~%33%Yellow ~Beige ~%33%Red}
"""

    assert [%{type: :short_answer_question,  }, %{type: :multiple_choice_question,  }] = :gift.parse(test_text)
  end

  test "escape left bracket" do
    p = :gift.parse("Can a \\{ be escaped?{=Yes a \\{ can ~No}")
    assert [%{type: :multiple_choice_question,  answers: [{"Yes a { can", 100}, {"No", 0}], text: "Can a { be escaped?", title: ""}] = p
  end

  test "escape right bracket" do
    p = :gift.parse("Can a \\} be escaped?{=Yes a \\} can ~No}")
    assert [%{type: :multiple_choice_question,  answers: [{"Yes a } can", 100}, {"No", 0}], text: "Can a } be escaped?", title: ""}] = p
  end

  test "escape colon" do
    p = :gift.parse("Can a \\: be escaped?{=Yes a \\: can ~No}")
    assert [%{type: :multiple_choice_question,  answers: [{"Yes a : can", 100}, {"No", 0}], text: "Can a : be escaped?", title: ""}] = p
  end

  test "escape hash" do
    p = :gift.parse("Can a \\# be escaped?{=Yes a \\# can ~No}")
    assert [%{type: :multiple_choice_question,  answers: [{"Yes a # can", 100}, {"No", 0}], text: "Can a # be escaped?", title: ""}] = p
  end

  test "escape tilde" do
    p = :gift.parse("Can a \\~ be escaped?{=Yes a \\~ can ~No}")
    assert [%{type: :multiple_choice_question,  answers: [{"Yes a ~ can", 100}, {"No", 0}], text: "Can a ~ be escaped?", title: ""}] = p
  end

  test "escape equals" do
    p = :gift.parse("Can a \\= be escaped?{=Yes a \\= can ~No}")
    assert [%{type: :multiple_choice_question,  answers: [{"Yes a = can", 100}, {"No", 0}], text: "Can a = be escaped?", title: ""}] = p
  end

  test "escapes in a comment" do
    p = parse_first("//Escapes in comments are redundant \\: since they end in a \n Question?{}")
    assert %{ type: :essay_question,  text: "Question?"} = p
  end

  test "crlf line breaks" do
    p = :gift.parse "Can we have DOS style line breaks?{\r\n=yes \r\n~no}\r\n \r\n And is it seen as a line_break?{TRUE}"
    assert [%{type: :multiple_choice_question,  }, %{ type: :true_false_question,  }] = p
  end

  test "can have line breaks after comments" do
    p = :gift.parse "//Comment\nWho's buried in Grant's tomb?{=Grant =Ulysses S. Grant =Ulysses Grant}"
    assert [%{type: :short_answer_question,  }] = p
  end

  test "dealing with blank lines at start" do
    p = :gift.parse "\n    \n//Comment\nWho's buried in Grant's tomb?{=Grant =Ulysses S. Grant =Ulysses Grant}"
    assert [%{type: :short_answer_question,  }] = p
  end

  test "dealing with blank lines at end" do
    p = :gift.parse "//Comment\nWho's buried in Grant's tomb?{=Grant =Ulysses S. Grant =Ulysses Grant}\n \n    \n "
    assert [%{type: :short_answer_question,  }] = p
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
    assert [%{type: :matching_question,  title: "Title"}] = p
  end

  test "single short answer with comment" do
    p = :gift.parse """
    // ===Short Answer===
    What is your favorite color?{=blue}
"""
    assert [%{type: :short_answer_question,  }] = p
  end

  test "single command" do
    p = :gift.parse("$COMMAND=1\n\nQuestion{}\n\n")

    assert [%{type: :command,  command: "COMMAND=1"}, %{ type: :essay_question,  text: "Question"}] = p
  end

  test "multiple commands" do
    p = :gift.parse("$CATEGORY=food\n$usecase=false\nQuestion{}\n\n")
    assert [%{type: :command,  command: "CATEGORY=food"}, %{type: :command,  command: "usecase=false"}, %{ type: :essay_question,  text: "Question"}] = p
  end

  test "markup type" do
    p = parse_first "[textile] This *essay* is marked up in textile.{}\n\n"
    assert %{ type: :essay_question,  text: "This *essay* is marked up in textile.", markup_language: :textile} = p
  end

  test "markup and title defaults" do
    p = parse_first "This essay is in plain text.{}\n\n"
    assert %{ type: :essay_question,  text: "This essay is in plain text.", markup_language: :plain, title: ""} = p
  end

  test "UTF 8 support" do
    p = parse_first "どこに行きますか？{=オーストラリア =Australia}"
    assert %{ type: :short_answer_question, text: "どこに行きますか？", answers: [{"オーストラリア", 100},{"Australia", 100}]}
  end
end
