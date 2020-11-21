# RabbitMQ

## setup

```
helm repo add bitnami https://charts.bitnami.com/bitnami
helm install salz-rmq bitnami/rabbitmq
```

Credentials:

    echo "Username      : user"
    echo "Password      : $(kubectl get secret --namespace default salz-rmq-rabbitmq -o jsonpath="{.data.rabbitmq-password}" | base64 --decode)"
    echo "ErLang Cookie : $(kubectl get secret --namespace default salz-rmq-rabbitmq -o jsonpath="{.data.rabbitmq-erlang-cookie}" | base64 --decode)"

RabbitMQ can be accessed within the cluster on port  at salz-rmq-rabbitmq.default.svc.

To access for outside the cluster, perform the following steps:

To Access the RabbitMQ AMQP port:

    echo "URL : amqp://127.0.0.1:5672/"
    kubectl port-forward --namespace default svc/salz-rmq-rabbitmq 5672:5672

To Access the RabbitMQ Management interface:

    echo "URL : http://127.0.0.1:15672/"
    kubectl port-forward --namespace default svc/salz-rmq-rabbitmq 15672:15672
