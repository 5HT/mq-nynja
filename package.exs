defmodule N2O.Mixfile do
  use Mix.Project

  def project do
    [app: :mq,
     version: "0.4.0",
     description: "Voxoz MQ",
     package: package,
     deps: deps]
  end

  def application do
    [mod: {:mq, []}]
  end

  defp package do
    [files: ["apps", "LICENSE", "README.md", "rebar.config", "sys.config", "vm.args"],
     licenses: ["APACHE"],
     maintainers: ["Namdak Tonpa"],
     name: :mq,
     links: %{"GitHub" => "https://github.com/voxoz/mq"}]
  end

  defp deps do
     [{:ex_doc, ">= 0.0.0", only: :dev}]
  end
end
