docker run -d --name connect_backend --rm -v $(pwd):/code -w /code -p 5000:5000 connect_backend rebar3 as dev shell
