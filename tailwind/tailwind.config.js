const plugin = require('tailwindcss/plugin');

module.exports = {
    mode: 'jit',
    theme: {
        extend: {
            colors: {
            }
        },
    },
    content: [
        "Web/Element/**/*.hs",
        "Web/View/**/*.hs",
        "static/*.js",
    ],
    safelist: [
        // Add custom class names.
        // https://tailwindcss.com/docs/content-configuration#safelisting-classes
    ],
    plugins: [
        require('@tailwindcss/forms'),
    ],
};