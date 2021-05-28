const defaultTheme = require('tailwindcss/defaultTheme')
const colors = require('tailwindcss/colors')

module.exports = {
  purge: {
    content: [
    './src/**/*.html',
    ],
  },
  theme: {
    extend: {
      fontFamily: {
        // sans: [
        //   'Inter',
        //   defaultTheme.fontFamily.sans,
        // ],
        // serif: [
        //   'Roboto Slab',
        //   defaultTheme.fontFamily.serif
        // ],
        'standout': [
            'Fira Code', 'monospace'
        ]
      },
      colors: {
          muted: "#f5f5f5",
          emphasis: "#444",
        // primary: 'var(--color-primary)',
        // secondary: 'var(--color-secondary)',
        // heading: 'var(--color-heading)',
        // body: 'var(--color-body)'
      },
    },
  },
  variants: {},
  plugins: [],
}
