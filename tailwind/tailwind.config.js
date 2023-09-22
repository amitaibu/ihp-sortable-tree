const plugin = require('tailwindcss/plugin');

module.exports = {
    mode: 'jit',
    theme: {
        extend: {
            colors: {
                'blue-gray': {
                    100: '#F1F5F9',
                    600: '#475569',
                },
                'pp': {
                    'purple': '#6002EE',
                    'teal': '#6DD7BA',

                },
                'teal': {
                    600: '#0D9488',
                }
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
        require('@tailwindcss/typography'),
    ],
};