defmodule Giftex do

  @doc """
    Import a GIFT formatted file.

    Returns {:ok, list_of_questions}
    or      {:error, reason}

  """
  def import_file(file) do
    case File.read(file) do
      {:ok, data} ->
          data
          |> parse
          |> handle_parse
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
    Parse a string.

    Returns a list of questions or in the case of a parse error, a tuple
    consisting of {parsed_questions, remaining_input, {{:line, line},{:column, column}}}

  """
  def parse(text) do
    text
    |> :gift.parse
    |> Giftex.Commands.process_commands
  end

  defp handle_parse(questions) when is_list(questions) do
    {:ok, questions}
  end

  defp handle_parse({_parsed, remaining,{{:line, line},{:column, column}}}) do
    [badline | _] = String.split(remaining, "\n")
    {:error, "Syntax error at line #{line}, column #{column} near:\n#{badline}"}
  end
end
