defmodule Giftex.Commands do
  def process_commands item_list do
    item_list
    |> categorize_question_list
    |> remove_unhandled_commands
  end

  defp categorize_question_list(qlist) when is_list(qlist) do
    {list, _} = Enum.map_reduce(qlist, [], fn(question, category) -> categorize(question, category) end)
    Enum.filter(list, fn(item) -> item != nil end)
  end

  defp categorize_question_list qlist do
    qlist
  end

  defp categorize %{command: <<"CATEGORY=">>}, _ do
    {nil, []}
  end

  defp categorize %{command: <<"CATEGORY=", category::binary>>}, _ do
    {nil, String.split(category, "/")}
  end

  defp categorize question, current_category do
    {Map.put(question, :category, current_category), current_category}
  end

  defp remove_unhandled_commands(qlist) when is_list(qlist) do
    Enum.filter qlist, fn(item) ->
      case item do
        %{command: command} -> false
        _ -> true
      end
    end
  end

  defp remove_unhandled_commands(qlist) do
    qlist
  end
end
