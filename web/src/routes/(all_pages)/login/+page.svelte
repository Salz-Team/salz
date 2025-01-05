<script lang="ts">
	import { page } from '$app/stores';
	import { settingsStore } from '$lib/stores/settings';
	import { onMount } from 'svelte';

	onMount(() => {
		const redirectUri = $page.url.searchParams.get('redirect_uri') ?? '/';

		const url =
			`${$settingsStore.apiBaseurl}/login` + (redirectUri ? `?redirect_uri=${redirectUri}` : '');

		window.location.href = url;
	});
</script>

<main>Logging in...</main>

<style>
	main {
		display: flex;
		justify-content: center;
		align-items: center;
	}
</style>
