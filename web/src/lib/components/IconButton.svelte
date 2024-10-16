<script lang="ts">
	import Icon from '@iconify/svelte';

	export let onClick: () => void = () => {};
	export let type: 'button' | 'reset' | 'submit' = 'button';
	export let isDisabled: boolean = false;
	let className: string = '';
	export { className as class };
	export let icon: string;
</script>

<button class={className} {type} on:click={onClick} disabled={isDisabled}>
	<Icon {icon} />
</button>

<style>
	button {
		border: none;
		background: none;
		display: flex;
		justify-content: center;
		align-items: center;
		cursor: pointer;
	}
</style>
