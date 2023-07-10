"use strict";
const input = document.getElementById("input")
const output = document.getElementById("output")
const update = document.getElementById("update")
const recolor = document.getElementById("recolor")
const canvas = document.getElementById("canvas")
const taste = document.getElementById("taste")

class Point {
	constructor(y, x) {
		this.y = y
		this.x = x
	}
}

class Attendee {
	constructor(y, x, tastes) {
		this.y = y
		this.x = x
		this.tastes = tastes
	}
}

class Pillar {
	constructor(y, x, r) {
		this.y = y
		this.x = x
		this.r = r
	}
}

class Input {
	constructor(json) {
		this.room_h = json.room_height
		this.room_w = json.room_width
		this.stage_h = json.stage_height
		this.stage_w = json.stage_width
		this.stage_b = json.stage_bottom_left[1]
		this.stage_l = json.stage_bottom_left[0]
		this.instruments = json.musicians
		this.attendees = json.attendees.map((v) => {
			return new Attendee(v.y, v.x, v.tastes)
		})
		this.pillars = json.pillars.map((v) => {
			return new Pillar(v.center[1], v.center[0], v.radius)
		})
	}
}

class Output {
	constructor(json) {
		this.ps = json.placements.map((v) => {
			return new Point(v.y, v.x)
		}) 
	}
}

class Visualizer {
	constructor() {
		this.input = null
		this.colorPalete = []
		this.recolor()
	}

	recolor() {
		this.colorPalete = []
		for (let i = 0; i < 1000; i++) {
			const col_r = Math.round(Math.random() * 150 + 50)
			const col_g = Math.round(Math.random() * 150 + 50)
			const col_b = Math.round(Math.random() * 150 + 50)
			this.colorPalete.push([col_r, col_g, col_b])
		}
	}

	update() {
		this.input = new Input(JSON.parse(input.value))
		this.output = new Output(JSON.parse(output.value))
		taste.max = this.input.attendees[0].tastes.length - 1
		this.show()
	}

	show() {
		const ctx = canvas.getContext('2d')
		const n = this.input.instruments.length
		const margin = 10
		const psize = 10
		const zoom = Math.min((canvas.height - margin * 2) / this.input.room_h, (canvas.width - margin * 2) / this.input.room_w)
		ctx.fillStyle = "black"
		ctx.fillRect(0, 0, canvas.width, canvas.height)
		ctx.translate(margin, canvas.height - margin)
		ctx.scale(zoom, -zoom)
		if (this.input.room_h < this.input.room_w) {
			ctx.translate(0, (this.input.room_w - this.input.room_h) / 2)
		} else {
			ctx.translate((this.input.room_h - this.input.room_w) / 2, 0)
		}
		ctx.fillStyle = "#EEE"
		ctx.fillRect(0, 0, this.input.room_w, this.input.room_h)
		ctx.fillStyle = "white"
		ctx.fillRect(this.input.stage_l, this.input.stage_b, this.input.stage_w, this.input.stage_h)

		ctx.strokeStyle = "black"
		ctx.fillStyle = "white"
		this.input.pillars.forEach((pl) => {
			ctx.beginPath()
			ctx.arc(pl.x, pl.y, pl.r, 0, 2 * Math.PI)
			ctx.stroke()
			ctx.fill()
		})

		const tastes = this.input.attendees.map(a => {return a.tastes[taste.value]})
		const min_taste = Math.min(...tastes)
		const max_taste = Math.max(...tastes)
		const limit = Math.max(Math.abs(max_taste), Math.abs(min_taste))
		const rgb = this.colorPalete[taste.value]
		console.log([min_taste, max_taste])
		this.input.attendees.forEach((att) => {
			const t = att.tastes[taste.value]
			const ratio = (t - min_taste) / (max_taste - min_taste)
			ctx.fillStyle = `rgb(${rgb[0] * ratio}, ${rgb[1] * ratio}, ${rgb[2] * ratio})`
			ctx.fillRect(att.x - psize, att.y - psize, psize * 2, psize * 2)
		}) 

		for (var i = 0; i < n; i++) {
			const rgb = this.colorPalete[this.input.instruments[i]]
			ctx.fillStyle = `rgb(${rgb[0]}, ${rgb[1]}, ${rgb[2]})`
			const p = this.output.ps[i]
			ctx.beginPath()
			ctx.arc(p.x, p.y, psize, 0, 2 * Math.PI)
			ctx.fill()
		}

		ctx.resetTransform()
	}
}

const visualizer = new Visualizer()

output.oninput = (event) => visualizer.update()
update.onclick = (event) => visualizer.update()
taste.onchange = (event) => visualizer.update()
recolor.onclick = (event) => {
	visualizer.recolor()
	visualizer.show()
}
