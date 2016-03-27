defmodule Giftex.Supervisor do
  use Supervisor

  def init [] do
    children = [
        worker(QuestionBank, :ets.new(:giftex))
      ]

    supervise(children, strategy: :one_for_one)
  end
end
