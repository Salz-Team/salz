from python:3.7

COPY Pipfile /
RUN pip install pipenv
RUN pipenv install

COPY src/ /app
WORKDIR /app


EXPOSE 8080

CMD ["/bin/bash", "run.sh"]
