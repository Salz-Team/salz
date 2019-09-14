from python:3.7

COPY . /app
WORKDIR /app

RUN pip install pipenv
RUN pipenv install

EXPOSE 8080

CMD ["/bin/bash", "run.sh"]
