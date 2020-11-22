#!/usr/bin/env bash

export RMQUSER="user"
export RMQPWD="$(kubectl get secret --namespace default salz-rmq-rabbitmq -o jsonpath="{.data.rabbitmq-password}" | base64 --decode)"
export RMQPORT="5672"
export PSQLUSER="postgres"
export PSQLPWD="postgres"
export PSQLDB="postgres"
export PSQLPORT="5432"

kubectl port-forward svc/salz-postgres-postgresql $PSQLPORT &
kubectl port-forward --namespace default svc/salz-rmq-rabbitmq $RMQPORT:$RMQPORT &
kubectl port-forward --namespace default svc/salz-rmq-rabbitmq $RMQMANPORT:$RMQMANPORT &
