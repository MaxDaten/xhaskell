/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [],
  theme: {
    extend: {},
  },
  plugins: [
    require('@tailwindcss/forms'),
    require('@tailwindcss/aspect-ratio'),
    require('@tailwindcss/language-server'),
    require('@tailwindcss/line-clamp'),
    require('@tailwindcss/typography'),
  ],
}

