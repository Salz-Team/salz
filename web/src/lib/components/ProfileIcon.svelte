<script lang="ts">
	interface Props {
		src: string;
		alt: string;
	}

	let { src, alt }: Props = $props();
</script>

<img {src} {alt} />

<style>
	img {
		border-radius: 50%;
		width: 120px;
		height: 120px;
	}
</style>
