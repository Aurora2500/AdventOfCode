#include <vector>
#include <cmath>
#include <array>
#include <string>
#include <iostream>
#include <memory>
#include <algorithm>

#include "visual.hpp"
#include "util.hpp"

struct pos
{
	std::array<int, 3> coords;

	int &x() { return coords[0]; }
	int &y() { return coords[1]; }
	int &z() { return coords[2]; }
	int x() const { return coords[0]; }
	int y() const { return coords[1]; }
	int z() const { return coords[2]; }

	bool operator==(const pos &other) const
	{
		return coords == other.coords;
	}

	bool operator<(const pos &other) const
	{
		return coords < other.coords;
	}

	pos operator+(const pos &other) const
	{
		pos result;
		for (int i = 0; i < 3; i++)
			result.coords[i] = coords[i] + other.coords[i];
		return result;
	}
};

struct cube
{
	pos p;
	pos size;
};

std::vector<cube> parse(std::istream &is)
{
	std::vector<cube> results;
	std::string line;
	while (std::getline(is, line))
	{
		std::string_view sv(line);

		cube c;

		c.p.x() = take_int(sv);
		sv.remove_prefix(1);
		c.p.y() = take_int(sv);
		sv.remove_prefix(1);
		c.p.z() = take_int(sv);
		sv.remove_prefix(1);

		c.size.x() = take_int(sv);
		sv.remove_prefix(1);
		c.size.y() = take_int(sv);
		sv.remove_prefix(1);
		c.size.z() = take_int(sv);

		c.size.x() = c.size.x() - c.p.x() + 1;
		c.size.y() = c.size.y() - c.p.y() + 1;
		c.size.z() = c.size.z() - c.p.z() + 1;
		results.push_back(c);
	}
	return results;
}

nm::float3 cube_middle(const std::vector<cube> &cubes)
{
	nm::float3 min = {999.0, 999.0, 999.0}, max = {-999.0, -999.0, -999.0};
	for (const auto &c : cubes)
	{
		min[0] = std::min(min[0], (float)c.p.x());
		min[1] = std::min(min[1], (float)c.p.y());
		min[2] = std::min(min[2], (float)c.p.z());
		max[0] = std::max(max[0], (float)c.p.x() + c.size.x());
		max[1] = std::max(max[1], (float)c.p.y() + c.size.y());
		max[2] = std::max(max[2], (float)c.p.z() + c.size.z());
	}
	std::cout << "min z: " << min.z() << "   max z: " << max.z() << std::endl;
	return (min + max) / 2.0f;
}

int main()
{
	auto cubes = parse(std::cin);
	auto midpoint = cube_middle(cubes);

	vis::SDL sdl;
	std::array<SDL_Cursor *, 2> cursors = {
			SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_SIZEALL),
			SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_ARROW),
	};
	bool running = true;
	vis::Window window("Advent of Code 2023 - Day 22", 1920, 1080);
	vis::Camera camera;
	camera.m_distance = 50.0;
	camera.m_position = midpoint;
	SDL_SetCursor(cursors[1]);
	bool mouse_down = false;
	while (running)
	{
		// poll events
		running = window.poll(
				[&](SDL_Event &e)
				{
					if (e.type == SDL_MOUSEBUTTONDOWN)
					{
						mouse_down = true;
						SDL_SetCursor(cursors[0]);
					}
					else if (e.type == SDL_MOUSEBUTTONUP)
					{
						mouse_down = false;
						SDL_SetCursor(cursors[1]);
					}
					else if (e.type == SDL_MOUSEMOTION && mouse_down)
					{
						float dx = e.motion.xrel / 300.0f;
						float dy = e.motion.yrel / 300.0f;
						camera.m_yaw += dx * camera.m_distance / 10.0;
						camera.m_yaw = std::fmod(camera.m_yaw, 2 * M_PI);
						camera.m_pitch += dy * camera.m_distance / 10.0;
						constexpr float max_pitch = M_PI / 2.0f - 0.01f;
						camera.m_pitch = std::clamp(camera.m_pitch, -max_pitch, max_pitch);
					}
					else if (e.type == SDL_MOUSEWHEEL)
					{
						camera.m_distance *= std::pow(1.1f, -e.wheel.y);
						camera.m_distance = std::clamp(camera.m_distance, 0.1f, 500.0f);
					}
				});
		window.clear();
		for (auto &cube : cubes)
		{
			auto s = cube.p;
			auto e = cube.size + cube.p;
			window.draw_line({(float)s.x(), (float)s.y(), (float)s.z()}, {(float)e.x(), (float)s.y(), (float)s.z()}, camera);
			window.draw_line({(float)s.x(), (float)s.y(), (float)s.z()}, {(float)s.x(), (float)e.y(), (float)s.z()}, camera);
			window.draw_line({(float)e.x(), (float)e.y(), (float)s.z()}, {(float)e.x(), (float)s.y(), (float)s.z()}, camera);
			window.draw_line({(float)e.x(), (float)e.y(), (float)s.z()}, {(float)s.x(), (float)e.y(), (float)s.z()}, camera);
			window.draw_line({(float)s.x(), (float)s.y(), (float)e.z()}, {(float)e.x(), (float)s.y(), (float)e.z()}, camera);
			window.draw_line({(float)s.x(), (float)s.y(), (float)e.z()}, {(float)s.x(), (float)e.y(), (float)e.z()}, camera);
			window.draw_line({(float)e.x(), (float)e.y(), (float)e.z()}, {(float)e.x(), (float)s.y(), (float)e.z()}, camera);
			window.draw_line({(float)e.x(), (float)e.y(), (float)e.z()}, {(float)s.x(), (float)e.y(), (float)e.z()}, camera);
			window.draw_line({(float)s.x(), (float)s.y(), (float)s.z()}, {(float)s.x(), (float)s.y(), (float)e.z()}, camera);
			window.draw_line({(float)e.x(), (float)s.y(), (float)s.z()}, {(float)e.x(), (float)s.y(), (float)e.z()}, camera);
			window.draw_line({(float)s.x(), (float)e.y(), (float)s.z()}, {(float)s.x(), (float)e.y(), (float)e.z()}, camera);
			window.draw_line({(float)e.x(), (float)e.y(), (float)s.z()}, {(float)e.x(), (float)e.y(), (float)e.z()}, camera);
		}
		window.show();
	}

	for (auto cursor : cursors)
		SDL_FreeCursor(cursor);
}