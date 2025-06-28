<script lang="ts">
        import { browser } from '$app/env'
	import { goto } from '$app/navigation';
	import { page } from '$app/stores';
	import { getMe } from '$lib/api/api';
	import { settingsStore } from '$lib/stores/settings';
	import { userStore, isLoggedIn } from '$lib/stores/user';
	import { onMount } from 'svelte';

	onMount(async () => {
		if (isLoggedIn) {
		        if(browser){
				goto('/');
			}
		}

		let redirectUrl = decodeURIComponent($page.url.searchParams.get('redirect_uri') ?? '%2F');
		if (redirectUrl === '') {
			redirectUrl = '/';
		}
		const userResult = await getMe($settingsStore.apiBaseurl);
		if (userResult.isOk()) {
			const userResponse = userResult.unwrap();
			const userid: string = userResponse.body['userid'] as string;
			const username: string = userResponse.body['username'] as string;
			const iconUrl: string = userResponse.body['iconpath'] as string;

			userStore.set({ id: userid, username, iconUrl });

		        if(browser){
				goto(redirectUrl);
			}
		}
	});
</script>

<main>Getting your credentials...</main>

<style>
	main {
		display: flex;
		justify-content: center;
		align-items: center;
	}
</style>
