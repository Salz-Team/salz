# Code Standards

## Formatting

Let Prettier do its job.

## Svelte files

### The `class` for custom components

If you'd like to set up the `class` attribute as a prop, use `class` instead of
`className` as you'd find in other frameworks like React. You can do that with
the following trick.

```svelte
<script lang="ts">
	let className: string = '';
	export { className as class };
</script>
```

If you need to combine this value with some other computed class name to be
attahced to your component, create a computed value. For example,

```svelte
<script lang="ts">
	let className: string = '';
	export { className as class };
	export let brightness: 'bright' | 'normal' | 'dark' = 'dark';

	$: computedClass = [className, brightness].join(' ');
</script>
```
