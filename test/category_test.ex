defmodule Giftex.CategoryTest do

  use ExUnit.Case

  setup_all do
    {:ok, data} = File.read("#{__DIR__}/support/categories.txt")
    gift = Giftex.parse(data)
    IO.inspect gift
    {:ok, gift: Giftex.parse(data)}
  end

  test "Question with no category", ctx do
    assert %{category: []} = Enum.at ctx.gift, 0
  end

  test "Question with a category", ctx do
    assert %{category: ["animals"]} = Enum.at ctx.gift, 1
  end

  test "Question with another category", ctx do
    assert %{category: ["colors"]} = Enum.at ctx.gift, 2
  end

  test "Question with a sub category", ctx do
    assert %{category: ["numbers", "small"]} = Enum.at ctx.gift, 3
  end

  test "Question with another sub category", ctx do
    assert %{category: ["numbers", "universal"]} = Enum.at ctx.gift, 4
  end
end
