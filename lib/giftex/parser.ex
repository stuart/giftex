defmodule Giftex.Parser do
  def build do
    :neotoma.file('src/gift.peg', transform_module: :gift_transform)
  end
end
