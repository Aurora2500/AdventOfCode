#include <SDL2/SDL.h>
#include <SDL2/SDL_opengl.h>
#include <GL/gl.h>
#include <functional>

#include "nicemath.h"
namespace vis
{
	class Camera
	{
	public:
		nm::float3 m_position;
		float m_pitch, m_yaw, m_distance;
		static constexpr float min_distance = 0.1f, max_distance = 1000.0f;

		Camera();
		nm::float3 project(nm::float3);
	};

	class SDL
	{
	public:
		SDL()
		{
			SDL_Init(SDL_INIT_EVERYTHING);
		}

		~SDL()
		{
			SDL_Quit();
		}
	};

	class Window
	{
		SDL_Window *m_window;
		SDL_Renderer *m_renderer;
		SDL_GLContext m_context;
		int m_width, m_height;

	public:
		Window(std::string &&title, int width, int height)
		{
			SDL_WindowFlags flags = (SDL_WindowFlags)(SDL_WINDOW_SHOWN | SDL_WINDOW_OPENGL);
			m_window = SDL_CreateWindow(title.c_str(), SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, width, height, SDL_WINDOW_SHOWN);
			m_context = SDL_GL_CreateContext(m_window);
			m_renderer = SDL_CreateRenderer(m_window, -1, SDL_RENDERER_ACCELERATED);
			// SDL_SetRenderDrawColor(m_renderer, 13, 77, 117, 255);
			SDL_UpdateWindowSurface(m_window);
			SDL_ShowWindow(m_window);
			SDL_GetWindowSize(m_window, &m_width, &m_height);
		}

		~Window()
		{
			SDL_DestroyRenderer(m_renderer);
			SDL_DestroyWindow(m_window);
		}

		void clear()
		{
			glViewport(0, 0, m_width, m_height);
			glClearColor(13.0 / 255.0, 77.0 / 255.0, 117.0 / 255.0, 1.0);
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		}

		void show()
		{
			SDL_RenderPresent(m_renderer);
			SDL_GL_SwapWindow(m_window);
		}

		std::pair<int, int> clip_to_screen(nm::float2 p)
		{
			p = (p + nm::float2(0.5f, 0.5f));
			return {(int)(p.x() * m_height + (m_width - m_height) / 2.0), (int)(p.y() * m_height)};
		}

		bool poll(std::function<void(SDL_Event &)> &&callback)
		{
			SDL_Event event;
			while (SDL_PollEvent(&event))
			{
				if (event.type == SDL_QUIT || (event.type == SDL_KEYDOWN && event.key.keysym.sym == SDLK_ESCAPE))
					return false;
				callback(event);
			}
			return true;
		}

		void draw_line(nm::float3 a, nm::float3 b, Camera &camera)
		{
			nm::float3 a2 = camera.project(a), b2 = camera.project(b);
			auto [x1, y1] = clip_to_screen(a2.xz());
			auto [x2, y2] = clip_to_screen(b2.xz());
			Uint8 oldr, oldg, oldb, olda;
			SDL_GetRenderDrawColor(m_renderer, &oldr, &oldg, &oldb, &olda);
			SDL_SetRenderDrawColor(m_renderer, 255, 255, 255, 255);
			SDL_RenderDrawLine(m_renderer, x1, y1, x2, y2);
			SDL_SetRenderDrawColor(m_renderer, oldr, oldg, oldb, olda);
		}
	};

	Camera::Camera()
	{
		m_position = {0, 0, 0};
		m_pitch = 0;
		m_yaw = 0;
		m_distance = 10;
	}

	nm::float3 Camera::project(nm::float3 p)
	{
		nm::float3 normalized = p - m_position;
		nm::float3 rotated;
		rotated[0] = normalized.x() * cos(m_yaw) - normalized.y() * sin(m_yaw);
		rotated[1] = normalized.x() * sin(m_yaw) + normalized.y() * cos(m_yaw);
		rotated[2] = normalized.z();

		nm::float3 rotated2;

		rotated2[0] = rotated.x();
		rotated2[1] = rotated.y() * cos(m_pitch) - rotated.z() * sin(m_pitch);
		rotated2[2] = -(rotated.y() * sin(m_pitch) + rotated.z() * cos(m_pitch));

		return rotated2 / m_distance;
	}

}
