defmodule Giftex do
  def import_file(file) do
    case File.read(file) do
      {:ok, data} ->
          data
          |> parse
          |> handle_parse
      {:error, reason} -> {:error, reason}
    end
  end

  def parse(text) do
    :gift.parse(text)
  end

  def build_parser do
    :neotoma.file('src/gift.peg', transform_module: :gift_transform)
  end

  defp handle_parse(questions) when is_list(questions) do
    {:ok, questions}
  end

  defp handle_parse({parsed, remaining,{{:line, line},{:column, column}}}) do
    [badline | _] = String.split(remaining, "\n")
    {:error, "Syntax error at line #{line}, column #{column} near:\n#{badline}"}
  end
end
