# Postgresql

Using some random helm chart, cuz easy. 

https://github.com/cetic/helm-postgresql

## Initial setup:

```
helm repo add cetic https://cetic.github.io/helm-charts
helm repo update
```

```
helm install salz-postgres cetic/postgresql
```

Because it's just development, fuck everything, we'll just use the default credentials. 

To access on local machine:

```
kubectl port-forward svc/salz-postgres-postgresql 5432
```

Then access via psql:

```
psql postgresql://localhost:5432 -U postgres
```
