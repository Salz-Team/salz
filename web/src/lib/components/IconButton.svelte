<script lang="ts">
	import Icon from '@iconify/svelte';

	interface Props {
		onClick?: () => void;
		type?: 'button' | 'reset' | 'submit';
		isDisabled?: boolean;
		class?: string;
		icon: string;
	}

	let {
		onClick = () => {},
		type = 'button',
		isDisabled = false,
		class: className = '',
		icon
	}: Props = $props();
</script>

<button class={className} {type} onclick={onClick} disabled={isDisabled}>
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
