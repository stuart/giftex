defmodule Giftex.ParserTest do
  use ExUnit.Case
  alias Giftex.Parser

  setup_all do
    :ok = Parser.build
    IEx.Helpers.c("src/gift.erl")
    :ok
  end

  defp can_parse s do
    :ok = :gift.parse s
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
    assert %Gift.TrueFalseQuestion{feedback: "No one is buried in Grant's tomb."} = p
  end

  test "multiple choice question" do
    p = :gift.parse("What color is the sky?{ =Blue ~Green ~Red}")
    assert p == %Gift.MultipleChoiceQuestion{text: "What color is the sky?", answers: [{"Blue", :correct}, {"Green", :incorrect}, {"Red", :incorrect}]}
  end
end
