<script lang="ts">
	import { type OnlineState } from '$lib/components/types/OnlineStatus';
	import Icon from '@iconify/svelte';
	export let onlineState: OnlineState = 'unknown';
</script>

<aside
	class="api-online-status"
	class:is-online={onlineState === 'online'}
	class:is-offline={onlineState === 'offline'}
>
	<Icon icon="tabler:circle-filled" />
	{#if onlineState === 'online'}
		online
	{:else if onlineState === 'offline'}
		offline
	{:else}
		unknown
	{/if}
</aside>

<style>
	.api-online-status {
		display: inline-flex;
		align-items: center;
	}

	.api-online-status > :global(svg) {
		color: orange;
	}

	.api-online-status.is-offline > :global(svg) {
		color: red;
	}

	.api-online-status.is-online > :global(svg) {
		color: green;
	}
</style>
