package main

import (
	"context"
	"fmt"
	"path/filepath"

	// "time"
	v1 "k8s.io/api/apps/v1"
	corev1 "k8s.io/api/core/v1"

	// "k8s.io/apimachinery/pkg/api/errors"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/client-go/kubernetes"
	"k8s.io/client-go/rest"
	"k8s.io/client-go/tools/clientcmd"
	"k8s.io/client-go/util/homedir"
	"k8s.io/utils/pointer"
)

var ErrNoMatchingDeployment error = fmt.Errorf("No matching deployment")

func getClientSet(location string) (*kubernetes.Clientset, error) {
	var kubeconfig string
	var config *rest.Config
	var err error
	if location != "incluster" {
		if home := homedir.HomeDir(); home != "" {
			kubeconfig = filepath.Join(home, ".kube", "config")
		}

		// use the current context in kubeconfig
		config, err = clientcmd.BuildConfigFromFlags("", kubeconfig)
		if err != nil {
			panic(err.Error())
		}
	}
	clientset, err := kubernetes.NewForConfig(config)
	if err != nil {
		return nil, err
	}

	return clientset, nil
}

func getDeploymentByUserLabel(user string, clientset *kubernetes.Clientset) (v1.Deployment, error) {
	listopts := metav1.ListOptions{
		LabelSelector: fmt.Sprintf("user=%s", user),
	}
	deploys, err := clientset.AppsV1().Deployments("default").List(context.TODO(), listopts)
	if err != nil {
		fmt.Println("listing deployments failed")
		return v1.Deployment{}, err
	}

	// TODO: assert only one deploy
	if len(deploys.Items) > 0 {
		return deploys.Items[0], nil
	}

	return v1.Deployment{}, ErrNoMatchingDeployment
}

func updateDeploymentByUserLabel(user, image string, deploy v1.Deployment, client *kubernetes.Clientset) error {
	deploy.Spec.Template.Spec.Containers[0].Image = image
	_, updateErr := client.AppsV1().Deployments("default").Update(
		context.TODO(),
		&deploy,
		metav1.UpdateOptions{},
	)
	if updateErr != nil {
		fmt.Println("Error updating deployment: ", updateErr)
		return updateErr
	}
	return nil
}

func createNewDeployment(user, image string, client *kubernetes.Clientset) error {
	deploy := v1.Deployment{
		ObjectMeta: metav1.ObjectMeta{Name: fmt.Sprintf("%s-deployment", user)},
		Spec: v1.DeploymentSpec{
			Replicas: pointer.Int32Ptr(1),
			Selector: &metav1.LabelSelector{
				MatchLabels: map[string]string{"app": fmt.Sprintf("%s-bot", user)},
			},
			Template: corev1.PodTemplateSpec{
				metav1.ObjectMeta{
					Name:      fmt.Sprintf("%s-bot-deployment", user),
					Namespace: "default",
					Labels: map[string]string{
						"app":  fmt.Sprintf("%s-bot", user),
						"user": user,
					},
				},
				corev1.PodSpec{
					Containers: []corev1.Container{
						corev1.Container{
							Name:  fmt.Sprintf("%s-bot", user),
							Image: image,
						},
					},
				},
			},
		},
	}

	createopts := metav1.CreateOptions{}
	_, err := client.AppsV1().Deployments("default").Create(
		context.Background(),
		&deploy,
		createopts,
	)
	if err != nil {
		fmt.Printf("Error creating deployment for user %s: %v", user, err)
		return err
	}
	return nil
}
