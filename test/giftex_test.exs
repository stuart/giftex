defmodule GiftexTest do
  use ExUnit.Case
  doctest Giftex

  test "the parsing of a file" do
    {:ok, data} = File.read("#{__DIR__}/support/gift_test.txt")
    gift = Giftex.parse(data)

    assert [%{type: :description},
            %{type: :true_false_question},
            %{type: :multiple_choice_question},
            %{type: :fill_in_question},
            %{type: :matching_question},
            %{type: :numeric_question},
            %{type: :numeric_question},
            %{type: :numeric_question},
            %{type: :essay_question},
            %{type: :command},
            %{type: :short_answer_question},
            %{type: :multiple_choice_question}
            ] = gift
  end

  test "the parsing of a badly formatted file" do
    {:ok, data} = File.read("#{__DIR__}/support/gift_bad_test.txt")
    gift = Giftex.parse(data)

    assert {[_], remaining, {{:line, 5}, {:column, 1}}} = gift
  end

  test "importing a file" do
    assert {:ok, [%{type: :description} | _]} = Giftex.import_file("#{__DIR__}/support/gift_test.txt")
  end

  test "importing a bad file" do
    q = Giftex.import_file("#{__DIR__}/support/gift_bad_test.txt")
    assert {:error, "Syntax error at line 5, column 1 near:\n::Q2:: What's between orange and green in = the spectrum?{ =yellow ~red ~blue }"} = q
  end

  test "file error is passed on" do
    assert {:error, :enoent} = Giftex.import_file("foo.txt")
  end
end
