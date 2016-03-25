defmodule Mix.Tasks.Giftex.Build do
  use Mix.Task

  @shortdoc "Build the parser from the PEG file"

  def run(_) do
    :neotoma.file('src/gift.peg', transform_module: :gift_transform)
  end
end
