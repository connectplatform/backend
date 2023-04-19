#docker build -t connect docker/app
docker run -i --rm -v $(pwd):/code -v /opt/ci/ssh:/root/.ssh:ro -w /code connect_backend ant update_asn
docker run -i --rm -v $(pwd):/code -v /opt/ci/ssh:/root/.ssh:ro -w /code connect_backend ant update_translations
docker run -i --rm -v $(pwd):/code -v /opt/ci/ssh:/root/.ssh:ro -w /code connect_backend rebar3 as dev compile
docker run -i --rm -v $(pwd):/code -v /opt/ci/ssh:/root/.ssh:ro -w /code connect_backend rebar3 as dev release
