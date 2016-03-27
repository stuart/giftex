defmodule GiftexTest do
  use ExUnit.Case

  test "Adding a question" do
    Giftex.add_question %{type: :true_false_question, text: "Is true true?", answers: [{true, 100}]}
  end
end
