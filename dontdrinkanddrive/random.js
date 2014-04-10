var dict = ['... att varje timme kör 525 personer onyktra i trafiken.',
            '... att 75 människor dödas varje år i trafiken på grund av alkohol.',
            '... att 4.6 miljoner onyktra resor körs varje år.',
            '... att varje år skadas mer än 1000 personer allvarligt i trafikolycker med alkoholpåverkade inblandade.'
]

var imgs = ['static/imgs/car1.jpg',
            'static/imgs/car2.jpg',
            'static/imgs/car3.jpg'
]

function getRandomInt(min, max) {
    return Math.floor(Math.random() * (max - min + 1)) + min;
}

document.getElementById('info').innerHTML = dict[getRandomInt(0,3)]
document.getElementById('body').setAttribute('background', imgs[getRandomInt(0,2)])
