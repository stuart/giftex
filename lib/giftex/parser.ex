defmodule Giftex.Parser do
  def build do
    :neotoma.file('src/gift.peg')
  end
end
