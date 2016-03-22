defmodule Giftex.ParserTest do
  use ExUnit.Case
  alias Giftex.Parser

  setup_all do
    :ok = Parser.build
    IEx.Helpers.c("src/gift.erl")
    :ok
  end

  test "description question" do
    assert %Gift.Description{text: "This is a description."} = :gift.parse("This is a description.")
  end

  test "essay question" do
    assert %Gift.EssayQuestion{text: "Write an essay about something."} = :gift.parse("Write an essay about something.{}")
  end

  test "true false question" do
    assert %Gift.TrueFalseQuestion{text: "The sky is blue.", answer: :true} = :gift.parse("The sky is blue.{T}")
    assert %Gift.TrueFalseQuestion{text: "The sky is blue.", answer: :true} = :gift.parse("The sky is blue.{TRUE}")
    assert %Gift.TrueFalseQuestion{text: "The sky is green.", answer: :false} = :gift.parse("The sky is green.{F}")
    assert %Gift.TrueFalseQuestion{text: "The sky is green.", answer: :false} = :gift.parse("The sky is green.{FALSE}")
  end

  test "feedback on question" do
    p = :gift.parse("Grant is buried in Grant's tomb.{FALSE#No one is buried in Grant's tomb.}")
    assert p == %Gift.TrueFalseQuestion{text: "Grant is buried in Grant's tomb.",
                                        answer: {false, "No one is buried in Grant's tomb."}}
  end

  test "multiple choice question" do
    p = :gift.parse("What color is the sky?{ =Blue ~Green ~Red}")
    assert p == %Gift.MultipleChoiceQuestion{text: "What color is the sky?", answers: [{"Blue", 100}, {"Green", 0}, {"Red", 0}]}
  end

  test "multiple choice question with feedback" do
    p = :gift.parse "What color is the sky?{ = Blue#Right ~Green ~Red#Very wrong}"
    assert p == %Gift.MultipleChoiceQuestion{text: "What color is the sky?",
                                             answers: [{"Blue", 100, "Right"},
                                                       {"Green", 0},
                                                       {"Red", 0, "Very wrong"}]}
  end

  test "multiple choice on multiple lines" do
    p = :gift.parse "What color is the sky?{\n= Blue#Right\n~Green\n~Red\n#Very wrong}\n"

    assert p == %Gift.MultipleChoiceQuestion{text: "What color is the sky?",
                                             answers: [{"Blue", 100, "Right"},
                                                       {"Green", 0},
                                                       {"Red", 0, "Very wrong"}]}
  end

  test "multiple choice question with weight" do
    p = :gift.parse "Which of these are primary colors?{ ~%33%Blue ~%33%Yellow ~Beige ~%33.3%Red}"

    assert %Gift.MultipleChoiceQuestion{text: "Which of these are primary colors?",
                                        answers: [{"Blue", 33},
                                                  {"Yellow", 33},
                                                  {"Beige", 0},
                                                  {"Red", 33.3}]} = p
  end

  test "numeric question with tolerance" do
    p = :gift.parse "How many pounds in a kilogram?{#2.2:0.1}"

    assert %Gift.NumericQuestion{answers: [{{2.1,2.3000000000000003}, 100}], text: "How many pounds in a kilogram?"} = p
  end

  test "test numeric with multiple answers" do
    p = :gift.parse "What is the value of PI?{#3.1415 =%50%3.1 =%25%3 }"

    assert %Gift.NumericQuestion{answers: [{3.1415, 100}, {3.1, 50}, {3, 25}]} = p
  end

  test "test numeric negative numbers" do
    p = :gift.parse "Calculate 2 - 6.{#-4.0}"
    assert %Gift.NumericQuestion{answers: [{-4.0, 100}]} = p
  end

  test "test numeric range question" do
    p = :gift.parse "::Q5:: What is a number from 1 to 5? {#1..5}"
    assert %Gift.NumericQuestion{answers: [{{1,5}, 100}]} = p
  end

  test "short answer question" do
    p = :gift.parse "Who's buried in Grant's tomb?{=Grant =Ulysses S. Grant =Ulysses Grant}"
    assert %Gift.ShortAnswerQuestion{text: "Who's buried in Grant's tomb?",
                                     answers: [{"Grant", 100}, {"Ulysses S. Grant", 100}, {"Ulysses Grant", 100}]} = p
  end

  test "matching question" do
    p = :gift.parse """
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
    p = :gift.parse "Little {~blue =red ~green } riding hood.\n"
    assert %Gift.FillInQuestion{text: "Little _ riding hood.", answers: [{"blue", 0}, {"red", 100}, {"green", 0}]} = p
  end
end
