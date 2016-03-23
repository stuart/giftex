defmodule GiftexTest do
  use ExUnit.Case
  doctest Giftex

  test "the parsing a file" do
    {:ok, data} = File.read("#{__DIR__}/support/gift_test.txt")
    gift = :gift.parse(data)
    assert length(gift) == 9

    assert [  %Gift.TrueFalseQuestion{},
              %Gift.MultipleChoiceQuestion{},
              %Gift.FillInQuestion{},
              %Gift.MatchingQuestion{},
              %Gift.NumericQuestion{},
              %Gift.NumericQuestion{},
              %Gift.NumericQuestion{},
              %Gift.EssayQuestion{},
              %Gift.ShortAnswerQuestion{}
              ] = gift
  end
end
