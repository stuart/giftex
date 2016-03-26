defmodule Giftex.QuestionTest do
  use ExUnit.Case

  alias Giftex.Question

  test "Answering a true false question correctly" do
    q = %{type: :true_false_question, answers: [{:true, 100}]}
    assert Question.check_answer(q, :true) == 100
  end

  test "Answering a true false question incorrectly" do
    q = %{type: :true_false_question, answers: [{:true, 100}]}
    assert Question.check_answer(q, :false) == 0
  end

  test "Answering a multiple choice question correctly" do
    q = %{type: :multiple_choice_question, answers: [{"A", 100}, {"B", 0}]}
    assert Question.check_answer(q, "A") == 100
  end

  test "Answering a multiple choice question incorrectly" do
    q = %{type: :multiple_choice_question, answers: [{"A", 100}, {"B", 0}]}
    assert Question.check_answer(q, "B") == 0
  end

  test "Answering a multiple choice question with a nonexistent answer" do
    q = %{type: :multiple_choice_question, answers: [{"A", 100}, {"B", 0}]}
    assert Question.check_answer(q, "C") == 0
  end

  test "Answering a fill in question correctly" do
    q = %{type: :fill_in_question, answers: [{"A", 100}]}
    assert Question.check_answer(q, "A") == 100
  end

  test "Answering a fill in question incorrectly" do
    q = %{type: :fill_in_question, answers: [{"A", 100}]}
    assert Question.check_answer(q, "X") == 0
  end

  test "Answering a matching question correctly" do
    q = %{type: :matching_question, answers: [{"foo", "bar"}, {"bar", "baz"}]}
    assert Question.check_answer(q, [{"foo", "bar"}, {"bar", "baz"}]) == 100
  end

  test "Answering a matching question incorrectly" do
    q = %{type: :matching_question, answers: [{"foo", "bar"}, {"bar", "baz"}]}
    assert Question.check_answer(q, [{"foo", "baz"}, {"bar", "bar"}]) == 0
  end

  test "Answering a matching question with too few pairs" do
    q = %{type: :matching_question, answers: [{"foo", "bar"}, {"bar", "baz"}]}
    assert Question.check_answer(q, [{"foo", "bar"}]) == 0
  end

  test "Answering a matching question with too many pairs" do
    q = %{type: :matching_question, answers: [{"foo", "bar"}, {"bar", "baz"}]}
    assert Question.check_answer(q, [{"foo", "baz"}, {"bar", "bar"}, {"frob", "buzz"}]) == 0
  end

  test "Answering a numeric question correctly" do
    q = %{type: :numeric_question, answers: [{{1,10},100}]}
    assert Question.check_answer(q, 7) == 100
  end

  test "Answering a numeric question incorrectly" do
    q = %{type: :numeric_question, answers: [{{1,10},100}]}
    assert Question.check_answer(q, 11) == 0
    assert Question.check_answer(q, 0) == 0
    assert Question.check_answer(q, -2.34) == 0
  end

  test "Answering a numeric question with multiple choices correctly" do
    q = %{type: :numeric_question, answers: [{{3.1415,3.1416},100}, {{3,4}, 50}]}
    assert Question.check_answer(q, 3.14159) == 100
    assert Question.check_answer(q, 3.2) == 50
  end

  test "Answering a simple numeric question" do
    q = %{type: :numeric_question,  answers: [{2.2, 100}], text: "How many pounds in a kilogram?"}
    assert Question.check_answer(q, 2.2) == 100
  end

  test "Answering an essay question" do
    q = %{type: :essay}
    assert Question.check_answer(q, "I wrote something.") == 0
  end

  test "Answering a short answer question correctly" do
    q = %{type: :short_answer_question, answers: [{"Pink Floyd",100}]}
    assert Question.check_answer(q, "Pink Floyd") == 100
  end

  test "Answering a short answer question incorrectly" do
    q = %{type: :short_answer_question, answers: [{"Pink Floyd",100}]}
    assert Question.check_answer(q, "Genesis") == 0
  end

  test "Answering a multiple choice question that has feedback" do
    q = %{type: :multiple_choice_question, answers: [{"A", 100, "Correct"}, {"B", 0, "Wrong"}]}
    assert Question.check_answer(q, "A") == {100,"Correct"}
  end

  test "Answering a true false question with feedback" do
    q = %{type: :true_false_question, answers: [{true, 100, "Correct"},{false, 0, "Incorrect"}]}
    assert Question.check_answer(q, true) == {100,"Correct"}
    assert Question.check_answer(q, false) == {0,"Incorrect"}
  end

  test "Multiple choice question requiring multiple answers" do
    q = %{type: :multiple_choice_question,
          answers: [{"No one", -50}, {"Grant", 50}, {"Grant's wife", 50}, {"Grant's father", -50}]}
    assert Question.check_answer(q, ["Grant"]) == 50
    assert Question.check_answer(q, ["Grant", "Grant's wife"]) == 100
    assert Question.check_answer(q, ["Grant", "Grant's father"]) == 0
  end

  test "Multiple choice question requiring multiple answers with feedback" do
    q = %{type: :multiple_choice_question,
          answers: [{"No one", -50}, {"Grant", 50, "Grant is correct"}, {"Grant's wife", 50}, {"Grant's father", -50, "Not his father"}]}
    assert Question.check_answer(q, ["Grant", "Grant's father"]) == {0, ["Grant is correct", "Not his father"]}
  end
end
