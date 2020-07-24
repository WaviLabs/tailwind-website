module.exports = {
  purge: [],
  theme: {
    extend: {
      screens: {
        light: { raw: "(prefers-color-scheme: light)" },
        dark: { raw: "(prefers-color-scheme: dark)" }
      },
      colors: {
        blue: '#48b5e0',
        green: '#71eeb8',
        pearl: '#EAE0C8',
        coral: '#FF7F50',
        coral_pink: '#F08080',
        dark: '#00122E',
        dark_alt: '#454d62'
      }
    },
  },
  variants: {
    backgroundColor: ['dark', 'dark-hover', 'dark-group-hover', 'dark-even', 'dark-odd', 'hover'],
    borderColor: ['dark', 'dark-disabled', 'dark-focus', 'dark-focus-within'],
    textColor: ['dark', 'dark-hover', 'dark-active', 'dark-placeholder', 'hover']
  },
  plugins: [
    require('tailwindcss-dark-mode')()
  ],
}
