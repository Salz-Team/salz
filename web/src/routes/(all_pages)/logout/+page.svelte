<script lang="ts">
        import { browser } from '$app/env'
	import { goto } from '$app/navigation';
	import { userStore, initialValue } from '$lib/stores/user';

	$userStore = initialValue;

        if(browser){
		goto('/');
	}
</script>
